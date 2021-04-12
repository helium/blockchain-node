-module(bn_implicit_burn).

-include("bn_jsonrpc.hrl").

-behaviour(blockchain_follower).

-behavior(bn_jsonrpc_handler).

%% blockchain_follower
-export([
    requires_sync/0,
    requires_ledger/0,
    init/1,
    follower_height/1,
    load_chain/2,
    load_block/5,
    terminate/2
]).

%% jsonrpc_handler
-export([handle_rpc/2]).
%% api
-export([follower_height/0]).

-define(DB_FILE, "ledger.db").

-record(state, {
    db :: rocksdb:db_handle(),
    default :: rocksdb:cf_handle(),
    implicit_burn :: rocksdb:cf_handle()
}).

requires_ledger() -> true.

requires_sync() -> false.

init(Args) ->
    Dir = filename:join(proplists:get_value(base_dir, Args, "data"), ?DB_FILE),
    case load_db(Dir) of
        {error, {db_open, "Corruption:" ++ _Reason}} ->
            lager:error("DB could not be opened corrupted ~p, cleaning up", [_Reason]),
            ok = bn_db:clean_db(Dir),
            init(Args);
        {ok, State} ->
            persistent_term:put(?MODULE, State),
            {ok, State#state{}}
    end.

follower_height(#state{db = DB, default = DefaultCF}) ->
    case bn_db:get_follower_height(DB, DefaultCF) of
        {ok, Height} -> Height;
        {error, _} = Error -> ?jsonrpc_error(Error)
    end.

load_chain(_Chain, State = #state{}) ->
    {ok, State}.

load_block(_Hash, Block, _Sync, _Ledger, State = #state{}) ->
    BlockHeight = blockchain_block_v1:height(Block),
    Transactions = blockchain_block:transactions(Block),
    lager:info("Loading Block ~p (~p transactions)", [BlockHeight, length(Transactions)]),
    ok = save_transactions(BlockHeight, Transactions, State),
    {ok, State}.

terminate(_Reason, #state{db = DB}) ->
    rocksdb:close(DB).

%%
%% jsonrpc_handler
%%

handle_rpc(<<"implicit_burn_get">>, {Param}) ->
    Hash = ?jsonrpc_b64_to_bin(<<"hash">>, Param),
    {ok, State} = get_state(),
    case get_implicit_burn(Hash, State) of
        {ok, {ImplicitBurn}} ->
            Json = blockchain_ledger_implicit_burn_v1:to_json(ImplicitBurn, []);
        {error, not_found} ->
            ?jsonrpc_error({not_found, "No implicit burn for txn: ~p", [?BIN_TO_B64(Hash)]});
        {error, _} = Error ->
            ?jsonrpc_error(Error)
    end;
handle_rpc(_, _) ->
    ?jsonrpc_error(method_not_found).

%%
%% api
%%

follower_height() ->
    {ok, State} = get_state(),
    follower_height(State).

%%
%% Internal
%%

get_state() ->
    bn_db:get_state(?MODULE).

-spec get_implicit_burn(Hash :: binary(), #state{}) ->
    {ok, {blockchain_ledger_implicit_burn_v1:implicit_burn()}} | {error, term()}.
get_implicit_burn(Hash, #state{db = DB, implicit_burn = ImplicitBurnCF}) ->
    case rocksdb:get(DB, ImplicitBurnCF, Hash, []) of
        {ok, BinImplicitBurn} ->
            {ok, {blockchain_ledger_implicit_burn_v1:deserialize(BinImplicitBurn)}};
        not_found ->
            {error, not_found};
        Error ->
            Error
    end.

-spec load_db(file:filename_all()) -> {ok, #state{}} | {error, any()}.
load_db(Dir) ->
    case bn_db:open_db(Dir, ["default", "implicit_burn"]) of
        {error, _Reason} = Error ->
            Error;
        {ok, DB, [DefaultCF, ImplicitBurnCF]} ->
            State = #state{
                db = DB,
                implicit_burn = ImplicitBurnCF
            },
            compact_db(State),
            {ok, State}
    end.

compact_db(#state{db = DB, default = Default, implicit_burn = ImplicitBurnCF}) ->
    rocksdb:compact_range(DB, Default, undefined, undefined, []),
    rocksdb:compact_range(DB, ImplicitBurnCF, undefined, undefined, []),
    ok.
