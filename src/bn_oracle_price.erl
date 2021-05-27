-module(bn_oracle_price).

% -behavior(blockchain_follower).

-include("bn_jsonrpc.hrl").

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

%% jsonrpc
-export([handle_rpc/2]).
%% api
-export([get_oracle_price/1]).

-define(DB_FILE, "oracle_price.db").

-record(state, {
    db :: rocksdb:db_handle(),
    default :: rocksdb:cf_handle(),
    prices :: rocksdb:cf_handle()
}).

%%
%% blockchain_follower
%%
requires_sync() -> false.

requires_ledger() -> false.

init(Args) ->
    Dir = filename:join(proplists:get_value(base_dir, Args, "data"), ?DB_FILE),
    case load_db(Dir) of
        {error, {db_open, "Corruption:" ++ _Reason}} ->
            lager:error("DB could not be opened corrupted ~p, cleaning up", [_Reason]),
            ok = bn_db:clean_db(Dir),
            init(Args);
        {ok, State} ->
            persistent_term:put(?MODULE, State),
            {ok, State}
    end.

follower_height(#state{db = DB, default = DefaultCF}) ->
    case bn_db:get_follower_height(DB, DefaultCF) of
        {ok, Height} -> Height;
        {error, _} = Error -> ?jsonrpc_error(Error)
    end.

load_chain(_Chain, State = #state{}) ->
    {ok, State}.

load_block(_Hash, Block, _Sync, Ledger, State = #state{db = DB, default = DefaultCF}) ->
    Height = blockchain_block:height(Block),
    ok =
        case Ledger of
            undefined ->
                ok;
            _ ->
                {ok, Price} = blockchain_ledger_v1:current_oracle_price(Ledger),
                save_oracle_price(Height, Price, State)
        end,
    ok = bn_db:put_follower_height(DB, DefaultCF, Height),
    {ok, State}.

terminate(_Reason, #state{db = DB}) ->
    rocksdb:close(DB).

%%
%% jsonrpc_handler
%%

handle_rpc(<<"oracle_price_current">>, _Params) ->
    case get_oracle_price(current) of
        {ok, {Height, Price}} -> #{height => Height, price => Price};
        {error, _} = Error -> ?jsonrpc_error(Error)
    end;
handle_rpc(<<"oracle_price_get">>, {Param}) ->
    Height =
        case ?jsonrpc_get_param(<<"height">>, Param, false) of
            V when is_integer(V) -> V;
            _ -> ?jsonrpc_error({invalid_params, Param})
        end,
    case get_oracle_price(Height) of
        {ok, {_, Price}} ->
            #{height => Height, price => Price};
        {error, not_found} ->
            ?jsonrpc_error({not_found, "Price not found for: ~p", [Height]});
        {error, _} = Error ->
            ?jsonrpc_error(Error)
    end;
handle_rpc(_, _) ->
    ?jsonrpc_error(method_not_found).

%%
%% API
%%

-spec get_oracle_price(Height :: current | non_neg_integer()) ->
    {ok, {Height :: non_neg_integer(), Price :: non_neg_integer()}} | {error, term()}.
get_oracle_price(current) ->
    {ok, Height} = blockchain:height(blockchain_worker:blockchain()),
    case blockchain_ledger_v1:current_oracle_price(blockchain:ledger()) of
        {ok, Price} -> {ok, {Height, Price}};
        {error, _} = Error -> Error
    end;
get_oracle_price(Height) when is_integer(Height) ->
    {ok, State} = get_state(),
    case get_oracle_price(Height, State) of
        {ok, Price} -> {ok, {Height, Price}};
        {error, _} = Error -> Error
    end.

%%
%% internal
%%
get_state() ->
    bn_db:get_state(?MODULE).

-spec get_oracle_price(Height :: non_neg_integer(), #state{}) ->
    {ok, Price :: non_neg_integer()} | {error, term()}.
get_oracle_price(Height, #state{db = DB, prices = PricesCF}) ->
    case rocksdb:get(DB, PricesCF, <<Height:64/integer-unsigned-little>>, []) of
        {ok, <<Price:64/integer-unsigned-little>>} -> {ok, Price};
        not_found -> {error, not_found};
        Error -> Error
    end.

-spec save_oracle_price(Height :: non_neg_integer(), Price :: non_neg_integer(), #state{}) ->
    ok.
save_oracle_price(Height, Price, #state{db = DB, prices = PricesCF}) ->
    {ok, Batch} = rocksdb:batch(),
    ok = rocksdb:batch_put(
        Batch,
        PricesCF,
        <<Height:64/integer-unsigned-little>>,
        <<Price:64/integer-unsigned-little>>
    ),
    rocksdb:write_batch(DB, Batch, [{sync, true}]).

-spec load_db(Dir :: file:filename_all()) -> {ok, #state{}} | {error, any()}.
load_db(Dir) ->
    case bn_db:open_db(Dir, ["default", "prices"]) of
        {error, _Reason} = Error ->
            Error;
        {ok, DB, [DefaultCF, PricesCF]} ->
            State = #state{db = DB, default = DefaultCF, prices = PricesCF},
            compact_db(State),
            {ok, State}
    end.

compact_db(#state{db = DB, default = Default, prices = PricesCF}) ->
    rocksdb:compact_range(DB, Default, undefined, undefined, []),
    rocksdb:compact_range(DB, PricesCF, undefined, undefined, []),
    ok.
