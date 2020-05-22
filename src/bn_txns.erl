-module(bn_txns).

-include("bn_jsonrpc.hrl").

-behaviour(blockchain_follower).
-behavior(bn_jsonrpc_handler).

%% blockchain_follower
-export([requires_sync/0, requires_ledger/0,
         init/1, follower_height/1, load_chain/2, load_block/5, terminate/2]).
%% jsonrpc_handler
-export([handle_rpc/2]).
%% api
-export([follower_height/0]).


-define(DB_FILE, "transactions.db").
-define(HEIGHT_KEY, <<"height">>).

-record(state,
        {
         db :: rocksdb:db_handle(),
         default :: rocksdb:cf_handle(),
         transactions :: rocksdb:cf_handle()
        }).

requires_ledger() -> false.
requires_sync() -> false.

init(Args) ->
    Dir = filename:join(proplists:get_value(base_dir, Args, "data"), ?DB_FILE),
    case load_db(Dir) of
        {error, {db_open,"Corruption:" ++ _Reason}} ->
            lager:error("DB could not be opened corrupted ~p, cleaning up", [_Reason]),
            ok = bn_db:clean_db(Dir),
            init(Args);
        {ok, State} ->
            persistent_term:put(?MODULE, State),
            {ok, State#state{}}
    end.

follower_height(#state{db=DB, default=DefaultCF}) ->
    case rocksdb:get(DB, DefaultCF, ?HEIGHT_KEY, []) of
        {ok, <<Height:64/integer-unsigned-little>>} ->
            Height;
        not_found ->
            0;
        {error, _}=Error ->
            ?jsonrpc_error(Error)
    end.

load_chain(_Chain, State=#state{}) ->
    {ok, State}.

load_block(_Hash, Block, _Sync, _Ledger, State=#state{}) ->
    BlockHeight = blockchain_block_v1:height(Block),
    Transactions = blockchain_block:transactions(Block),
    lager:info("Loading Block ~p (~p transactions)", [BlockHeight, length(Transactions)]),
    ok = save_transactions(BlockHeight, Transactions, State),
    {ok, State}.

terminate(_Reason, #state{db=DB}) ->
    rocksdb:close(DB).


%%
%% jsonrpc_handler
%%

handle_rpc(<<"get_transaction">>, [Param]) ->
    Hash = ?B64_TO_BIN(Param),
    {ok, State} = get_state(),
    case get_transaction(Hash, State) of
        {ok, Txn} ->
            blockchain_txn:to_json(Txn, []);
        {error, not_found} ->
            ?jsonrpc_error({not_found, "No transaction: ~p", [Param]});
        {error, _}=Error ->
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
    case persistent_term:get(?MODULE, false) of
        false ->
            {error, {no_database, ?MODULE}};
        State ->
            {ok, State}
    end.

-spec get_transaction(Hash::binary(), #state{}) -> {ok, blockchain_txn:txn()} | {error, term()}.
get_transaction(Hash, #state{db=DB, transactions=TransactionsCF}) ->
    case rocksdb:get(DB, TransactionsCF, Hash, []) of
        {ok, BinTxn} ->
            {ok, blockchain_txn:deserialize(BinTxn)};
        not_found ->
            {error, not_found};
        Error ->
            Error
    end.

save_transactions(Height, Transactions, #state{db=DB, default=DefaultCF, transactions=TransactionsCF}) ->
    {ok, Batch} = rocksdb:batch(),
    lists:foreach(fun(Txn) ->
                          Hash = blockchain_txn:hash(Txn),
                          ok = rocksdb:batch_put(Batch, TransactionsCF, Hash, blockchain_txn:serialize(Txn))
                  end, Transactions),
    rocksdb:batch_put(Batch, DefaultCF, ?HEIGHT_KEY, <<Height:64/integer-unsigned-little>>),
    ok = rocksdb:write_batch(DB, Batch, [{sync, true}]).


-spec load_db(file:filename_all()) -> {ok, #state{}} | {error, any()}.
load_db(Dir) ->
    case bn_db:open_db(Dir, ["default", "transactions"]) of
        {error, _Reason}=Error ->
            Error;
        {ok, DB, [DefaultCF, TransactionsCF]} ->
            State = #state{
                       db=DB,
                       default=DefaultCF,
                       transactions=TransactionsCF
                      },
            compact_db(State),
            {ok, State}
    end.

compact_db(#state{db=DB, default=Default, transactions=TransactionsCF}) ->
    rocksdb:compact_range(DB, Default, undefined, undefined, []),
    rocksdb:compact_range(DB, TransactionsCF, undefined, undefined, []),
    ok.
