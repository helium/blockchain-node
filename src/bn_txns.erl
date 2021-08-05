-module(bn_txns).

-include("bn_jsonrpc.hrl").

% -behaviour(blockchain_follower).

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

-define(DB_FILE, "transactions.db").

-record(state, {
    db :: rocksdb:db_handle(),
    default :: rocksdb:cf_handle(),
    heights :: rocksdb:cf_handle(),
    transactions :: rocksdb:cf_handle(),
    json :: rocksdb:cf_handle()
}).

requires_ledger() -> false.

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
    Chain = blockchain_worker:blockchain(),
    ok = save_transactions(
        BlockHeight,
        Transactions,
        blockchain:ledger(Chain),
        Chain,
        State
    ),
    {ok, State}.

terminate(_Reason, #state{db = DB}) ->
    rocksdb:close(DB).

%%
%% jsonrpc_handler
%%

handle_rpc(<<"transaction_get">>, {Param}) ->
    Hash = ?jsonrpc_b64_to_bin(<<"hash">>, Param),
    {ok, State} = get_state(),
    case get_transaction_json(Hash, State) of
        {ok, Json} ->
            Json;
        {error, not_found} ->
            ?jsonrpc_error({not_found, "No transaction: ~p", [?BIN_TO_B64(Hash)]});
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

-spec get_transaction(Hash :: binary(), #state{}) ->
    {ok, {Height :: pos_integer() | undefined, blockchain_txn:txn()}} | {error, term()}.
get_transaction(Hash, #state{db = DB, heights = HeightsCF, transactions = TransactionsCF}) ->
    case rocksdb:get(DB, TransactionsCF, Hash, []) of
        {ok, BinTxn} ->
            Height =
                case rocksdb:get(DB, HeightsCF, Hash, []) of
                    not_found -> undefined;
                    {ok, <<H:64/integer-unsigned-little>>} -> H
                end,
            {ok, {Height, blockchain_txn:deserialize(BinTxn)}};
        not_found ->
            {error, not_found};
        Error ->
            Error
    end.

-spec get_transaction_json(Hash :: binary(), #state{}) ->
    {ok, blockchain_json:json_object()} | {error, term()}.
get_transaction_json(Hash, State = #state{db = DB, json = JsonCF}) ->
    Chain = blockchain_worker:blockchain(),
    case rocksdb:get(DB, JsonCF, Hash, []) of
        {ok, BinJson} ->
            Json = jsone:decode(BinJson, []),
            case blockchain:get_implicit_burn(Hash, Chain) of
                {ok, ImplicitBurn} ->
                    {ok, Json#{implicit_burn => blockchain_implicit_burn:to_json(ImplicitBurn, [])}};
                {error, _} ->
                    {ok, Json}
            end;
        not_found ->
            case get_transaction(Hash, State) of
                {ok, {Height, Txn}} ->
                    Json = blockchain_txn:to_json(Txn, []),
                    case blockchain:get_implicit_burn(Hash, Chain) of
                        {ok, ImplicitBurn} ->
                            {ok, Json#{block => Height, implicit_burn => blockchain_implicit_burn:to_json(ImplicitBurn, [])}};
                        {error, _} ->
                            {ok, Json#{block => Height}}
                    end;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

save_transactions(Height, Transactions, Ledger, Chain, #state{
    db = DB,
    default = DefaultCF,
    heights = HeightsCF,
    transactions = TransactionsCF,
    json = JsonCF
}) ->
    {ok, Batch} = rocksdb:batch(),
    HeightBin = <<Height:64/integer-unsigned-little>>,
    lists:foreach(
        fun(Txn) ->
            Hash = blockchain_txn:hash(Txn),
            Json =
                try
                    blockchain_txn:to_json(Txn, [{ledger, Ledger}, {chain, Chain}])
                catch
                    _:_ ->
                        blockchain_txn:to_json(Txn, [])
                end,
            ok = rocksdb:batch_put(
                Batch,
                TransactionsCF,
                Hash,
                blockchain_txn:serialize(Txn)
            ),
            ok = rocksdb:batch_put(
                Batch,
                JsonCF,
                Hash,
                jsone:encode(Json#{block => Height}, [undefined_as_null])
            ),
            ok = rocksdb:batch_put(Batch, HeightsCF, Hash, HeightBin)
        end,
        Transactions
    ),
    bn_db:batch_put_follower_height(Batch, DefaultCF, Height),
    rocksdb:write_batch(DB, Batch, [{sync, true}]).

-spec load_db(file:filename_all()) -> {ok, #state{}} | {error, any()}.
load_db(Dir) ->
    case bn_db:open_db(Dir, ["default", "heights", "transactions", "json"]) of
        {error, _Reason} = Error ->
            Error;
        {ok, DB, [DefaultCF, HeightsCF, TransactionsCF, JsonCF]} ->
            State = #state{
                db = DB,
                default = DefaultCF,
                heights = HeightsCF,
                transactions = TransactionsCF,
                json = JsonCF
            },
            compact_db(State),
            {ok, State}
    end.

compact_db(#state{db = DB, default = Default, transactions = TransactionsCF, json = JsonCF}) ->
    rocksdb:compact_range(DB, Default, undefined, undefined, []),
    rocksdb:compact_range(DB, TransactionsCF, undefined, undefined, []),
    rocksdb:compact_range(DB, JsonCF, undefined, undefined, []),
    ok.
