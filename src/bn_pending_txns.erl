-module(bn_pending_txns).

-behavior(blockchain_follower).

-include("bn_jsonrpc.hrl").

%% blockchain_follower
-export([requires_sync/0, requires_ledger/0,
         init/1, follower_height/1, load_chain/2, load_block/5, terminate/2]).
%% jsonrpc
-export([handle_rpc/2]).
%% API
-export([submit_txn/1, get_txn_status/1]).

-define(DB_FILE, "pendning_transactions.db").
-define(HEIGHT_KEY, <<"height">>).

-define(TXN_STATUS_CLEARED, 0).
-define(TXN_STATUS_FAILED, 1).

-record(state,
        {
         db :: rocksdb:db_handle(),
         default :: rocksdb:cf_handle(),
         pending :: rocksdb:cf_handle(),
         status :: rocksdb:cf_handle()
        }).

%%
%% blockchain_follower
%%
requires_sync() -> false.
requires_ledger() -> false.

init(Args) ->
    Dir = filename:join(proplists:get_value(base_dir, Args, "data"), ?DB_FILE),
    case load_db(Dir) of
        {error, {db_open,"Corruption:" ++ _Reason}} ->
            lager:error("DB could not be opened corrupted ~p, cleaning up", [_Reason]),
            ok = bn_db:clean_db(Dir),
            init(Args);
        {ok, State} ->
            persistent_term:put(?MODULE, State),
            {ok, State}
    end.

follower_height(#state{db=DB, default=DefaultCF}) ->
    case rocksdb:get(DB, DefaultCF, ?HEIGHT_KEY, []) of
        {ok, <<Height:64/integer-unsigned-little>>} ->
            bn_txns:snapshot_height(Height);
        not_found ->
            bn_txns:snapshot_height(0);
        {error, _}=Error ->
            ?jsonrpc_error(Error)
    end.

load_chain(_Chain, State=#state{}) ->
    Submitted = submit_pending_txns(State),
    lager:info("Submitted ~p pending transactions", [Submitted]),
    {ok, State}.

load_block(_Hash, Block, _Sync, _Ledger, State=#state{db=DB, default=DefaultCF}) ->
    BlockHeight = blockchain_block:height(Block),
    ok = rocksdb:put(DB, DefaultCF, ?HEIGHT_KEY, <<BlockHeight:64/integer-unsigned-little>>, []),
    {ok, State}.


terminate(_Reason, #state{db=DB}) ->
    rocksdb:close(DB).

%%
%% jsonrpc_handler
%%

handle_rpc(<<"pending_transaction_status">>, {Param}) ->
    Hash = ?jsonrpc_b64_to_bin(<<"hash">>, Param),
    {ok, State} = get_state(),
    case get_txn_status(Hash, State) of
        {ok, pending} ->
            <<"pending">>;
        {ok, {cleared, _}} ->
            <<"cleared">>;
        {ok, {failed, Reason}} ->
            Reason;
        {error, not_found} ->
            ?jsonrpc_error({not_found, "Pending transaction not found"})
    end;

handle_rpc(_, _) ->
    ?jsonrpc_error(method_not_found).

%%
%% API
%%
-spec submit_txn(blockchain_txn:txn()) -> {ok, Hash::binary()} | {error, term()}.
submit_txn(Txn) ->
    {ok, State} = get_state(),
    submit_txn(Txn, State).

-type txn_status() :: pending | {failed, Reason::binary()}.

-spec get_txn_status(Hash::binary()) -> {ok, txn_status()} | {error, term()}.
get_txn_status(Hash) ->
    {ok, State} = get_state(),
    get_txn_status(Hash, State).

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

-spec get_txn_status(Hash::binary(), #state{})
          -> {ok, {failed, Reason::binary()} | {cleared, Block::pos_integer()} | pending}
              | {error, not_found}.
get_txn_status(Hash, #state{db=DB, pending=PendingCF, status=StatusCF}) ->
    case rocksdb:get(DB, StatusCF, Hash, []) of
        {ok, <<(?TXN_STATUS_CLEARED):8, Block:64/integer-unsigned-little>>} ->
            {ok, {cleared, Block}};
        {ok, <<(?TXN_STATUS_FAILED):8, Reason/binary>>} ->
            {ok, {failed, Reason}};
        not_found ->
            case rocksdb:get(DB, PendingCF, Hash, []) of
                not_found ->
                    {error, not_found};
                {ok, _} ->
                    {ok, pending}
            end
    end.

-spec submit_pending_txns(#state{}) -> non_neg_integer().
submit_pending_txns(State=#state{db=DB, pending=PendingCF}) ->
    %% iterate over the transactions and submit each one of them
    {ok, Itr} = rocksdb:iterator(DB, PendingCF, []),
    Submitted = submit_pending_txns(Itr, rocksdb:iterator_move(Itr, first), State, 0),
    catch rocksdb:iterator_close(Itr),
    Submitted.

submit_pending_txns(_Itr, {error, _Error}, #state{}, Acc) ->
    Acc;
submit_pending_txns(Itr, {ok, Hash, BinTxn}, State=#state{}, Acc) ->
    try blockchain_txn:deserialize(BinTxn) of
        Txn ->
            blockchain_txn_mgr:submit(Txn,
                                      fun(Result) ->
                                              finalize_txn(Hash, Result, State)
                                      end)
    catch
        What:Why ->
            lager:warning("Error while fetching pending transaction: ~p: ~p ~p", [Hash, What, Why])
    end,
    submit_pending_txns(Itr, rocksdb:iterator_move(Itr, next), State, Acc+1).


finalize_txn(Hash, Status, #state{db=DB, pending=PendingCF, status=StatusCF}) ->
    {ok, Batch} = rocksdb:batch(),
    %% Set cleared or failed status
    case Status of
        ok ->
            rocksdb:batch_put(Batch, StatusCF, Hash, <<(?TXN_STATUS_CLEARED):8, 0:64/integer-unsigned-little>>);
        {error, Error} ->
            ErrorBin = list_to_binary(lists:flatten(io_lib:format("~p", [Error]))),
            rocksdb:batch_put(Batch, StatusCF, Hash, <<(?TXN_STATUS_FAILED):8, ErrorBin/binary>>)
    end,
    %% Delete the transaction from the pending table
    rocksdb:batch_delete(Batch, PendingCF, Hash),
    ok = rocksdb:write_batch(DB, Batch, [{sync, true}]).


submit_txn(Txn, State=#state{db=DB, pending=PendingCF}) ->
    {ok, Batch} = rocksdb:batch(),
    Hash = blockchain_txn:hash(Txn),
    ok = rocksdb:batch_put(Batch, PendingCF, Hash, blockchain_txn:serialize(Txn)),
    ok = rocksdb:write_batch(DB, Batch, [{sync, true}]),
    blockchain_txn_mgr:submit(Txn,
                              fun(Result) ->
                                      finalize_txn(Hash, Result, State)
                              end),
    {ok, Hash}.


-spec load_db(Dir::file:filename_all()) -> {ok, #state{}} | {error, any()}.
load_db(Dir) ->
    case bn_db:open_db(Dir, ["default", "pending", "status"]) of
        {error, _Reason}=Error ->
            Error;
        {ok, DB, [DefaultCF, PendingCF, StatusCF]} ->
            State = #state{
                       db=DB,
                       default=DefaultCF,
                       pending=PendingCF,
                       status=StatusCF
                      },
            compact_db(State),
            {ok, State}
    end.


compact_db(#state{db=DB, default=Default, pending=PendingCF, status=StatusCF}) ->
    rocksdb:compact_range(DB, Default, undefined, undefined, []),
    rocksdb:compact_range(DB, PendingCF, undefined, undefined, []),
    rocksdb:compact_range(DB, StatusCF, undefined, undefined, []),
    ok.
