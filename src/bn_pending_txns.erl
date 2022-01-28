-module(bn_pending_txns).

% -behavior(blockchain_follower).

-include("bn_jsonrpc.hrl").

-include_lib("helium_proto/include/blockchain_txn_pb.hrl").

-type supported_txn() ::
    #blockchain_txn_oui_v1_pb{}
    | #blockchain_txn_routing_v1_pb{}
    | #blockchain_txn_vars_v1_pb{}
    | #blockchain_txn_add_gateway_v1_pb{}
    | #blockchain_txn_assert_location_v1_pb{}
    | #blockchain_txn_assert_location_v2_pb{}
    | #blockchain_txn_payment_v1_pb{}
    | #blockchain_txn_payment_v2_pb{}
    | #blockchain_txn_create_htlc_v1_pb{}
    | #blockchain_txn_redeem_htlc_v1_pb{}
    | #blockchain_txn_price_oracle_v1_pb{}
    | #blockchain_txn_token_burn_v1_pb{}
    | #blockchain_txn_transfer_hotspot_v1_pb{}
    | #blockchain_txn_transfer_hotspot_v2_pb{}
    | #blockchain_txn_security_exchange_v1_pb{}
    | #blockchain_txn_stake_validator_v1_pb{}
    | #blockchain_txn_unstake_validator_v1_pb{}
    | #blockchain_txn_transfer_validator_stake_v1_pb{}
    | #blockchain_txn_state_channel_open_v1_pb{}.

-type nonce_type() :: none | balance | gateway | security | dc.
-type nonce_address() :: libp2p_crypto:pubkey_bin() | undefined.
-type nonce() :: non_neg_integer().

-export_type([nonce_type/0, nonce_address/0, nonce/0]).

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
%% API
-export([submit_txn/1, get_txn_status/1, get_max_nonce/2]).

-define(DB_FILE, "pendning_transactions.db").
-define(TXN_STATUS_CLEARED, 0).
-define(TXN_STATUS_FAILED, 1).

-record(state, {
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
    Submitted = submit_pending_txns(State),
    lager:info("Submitted ~p pending transactions", [Submitted]),
    {ok, State}.

load_block(_Hash, Block, _Sync, _Ledger, State = #state{db = DB, default = DefaultCF}) ->
    ok = bn_db:put_follower_height(DB, DefaultCF, blockchain_block:height(Block)),
    {ok, State}.

terminate(_Reason, #state{db = DB}) ->
    rocksdb:close(DB).

%%
%% jsonrpc_handler
%%

handle_rpc(<<"pending_transaction_get">>, {Param}) ->
    Hash = ?jsonrpc_b64_to_bin(<<"hash">>, Param),
    {ok, State} = get_state(),
    case get_txn_status(Hash, State) of
        {error, not_found} ->
            ?jsonrpc_error({not_found, "Pending transaction not found"});
        {ok, Status} ->
            Json =
                case get_txn(Hash, State) of
                    {error, not_found} ->
                        %% transaction was cleared or failed, leave the txn
                        %% details out
                        #{};
                    {ok, Txn} ->
                        #{<<"txn">> => blockchain_txn:to_json(Txn, [])}
                end,
            case Status of
                pending ->
                    Json#{
                        <<"status">> => <<"pending">>
                    };
                {cleared, Height} ->
                    Json#{
                        <<"status">> => <<"cleared">>,
                        <<"block">> => Height
                    };
                {failed, Reason} ->
                    Json#{
                        <<"status">> => <<"failed">>,
                        <<"failed_reason">> => Reason
                    }
            end
    end;
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
handle_rpc(<<"pending_transaction_submit">>, {Param}) ->
    BinTxn = ?jsonrpc_b64_to_bin(<<"txn">>, Param),
    try
        Txn = blockchain_txn:deserialize(BinTxn),
        {ok, _} = submit_txn(Txn),
        blockchain_txn:to_json(Txn, [])
    catch
        _:_ -> ?jsonrpc_error(invalid_params)
    end;
handle_rpc(_, _) ->
    ?jsonrpc_error(method_not_found).

%%
%% API
%%
-spec submit_txn(blockchain_txn:txn()) -> {ok, Hash :: binary()} | {error, term()}.
submit_txn(Txn) ->
    {ok, State} = get_state(),
    submit_txn(Txn, State).

-type txn_status() :: pending | {failed, Reason :: binary()}.

-spec get_txn_status(Hash :: binary()) -> {ok, txn_status()} | {error, term()}.
get_txn_status(Hash) ->
    {ok, State} = get_state(),
    get_txn_status(Hash, State).

%%
%% Internal
%%

get_state() ->
    bn_db:get_state(?MODULE).

-spec get_txn_status(Hash :: binary(), #state{}) ->
    {ok, {failed, Reason :: binary()} | {cleared, Block :: pos_integer()} | pending}
    | {error, not_found}.
get_txn_status(Hash, #state{db = DB, pending = PendingCF, status = StatusCF}) ->
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
submit_pending_txns(State = #state{db = DB, pending = PendingCF}) ->
    %% iterate over the transactions and submit each one of them
    {ok, Itr} = rocksdb:iterator(DB, PendingCF, []),
    Submitted = submit_pending_txns(Itr, rocksdb:iterator_move(Itr, first), State, 0),
    catch rocksdb:iterator_close(Itr),
    Submitted.

submit_pending_txns(_Itr, {error, _Error}, #state{}, Acc) ->
    Acc;
submit_pending_txns(Itr, {ok, Hash, BinTxn}, State = #state{}, Acc) ->
    try blockchain_txn:deserialize(BinTxn) of
        Txn ->
            blockchain_txn_mgr:submit(Txn, fun(Result) ->
                finalize_txn(Hash, Result, State)
            end)
    catch
        What:Why ->
            lager:warning("Error while fetching pending transaction: ~p: ~p ~p", [
                Hash,
                What,
                Why
            ])
    end,
    submit_pending_txns(Itr, rocksdb:iterator_move(Itr, next), State, Acc + 1).

finalize_txn(Hash, Status, #state{db = DB, pending = PendingCF, status = StatusCF}) ->
    {ok, Batch} = rocksdb:batch(),
    %% Set cleared or failed status
    case Status of
        ok ->
            rocksdb:batch_put(
                Batch,
                StatusCF,
                Hash,
                <<(?TXN_STATUS_CLEARED):8, 0:64/integer-unsigned-little>>
            );
        {error, Error} ->
            ErrorBin = list_to_binary(lists:flatten(io_lib:format("~p", [Error]))),
            rocksdb:batch_put(
                Batch,
                StatusCF,
                Hash,
                <<(?TXN_STATUS_FAILED):8, ErrorBin/binary>>
            )
    end,
    %% Delete the transaction from the pending table
    rocksdb:batch_delete(Batch, PendingCF, Hash),
    ok = rocksdb:write_batch(DB, Batch, [{sync, true}]).

submit_txn(Txn, State = #state{db = DB, pending = PendingCF}) ->
    {ok, Batch} = rocksdb:batch(),
    Hash = blockchain_txn:hash(Txn),
    ok = rocksdb:batch_put(Batch, PendingCF, Hash, blockchain_txn:serialize(Txn)),
    ok = rocksdb:write_batch(DB, Batch, [{sync, true}]),
    blockchain_txn_mgr:submit(Txn, fun(Result) ->
        finalize_txn(Hash, Result, State)
    end),
    {ok, Hash}.

-spec get_txn(Hash :: binary(), #state{}) -> {ok, blockchain_txn:txn()} | {error, term()}.
get_txn(Hash, #state{db = DB, pending = PendingCF}) ->
    case rocksdb:get(DB, PendingCF, Hash, []) of
        {ok, BinTxn} ->
            {ok, blockchain_txn:deserialize(BinTxn)};
        not_found ->
            {error, not_found};
        Error ->
            Error
    end.

-spec get_max_nonce(nonce_address(), nonce_type()) -> nonce().
get_max_nonce(Address, NonceType) ->
    {ok, State} = get_state(),
    get_max_nonce(Address, NonceType, State).

-spec get_max_nonce(nonce_address(), nonce_type(), #state{}) -> nonce().
get_max_nonce(Address, NonceType, #state{db = DB, pending = PendingCF}) ->
    {ok, Itr} = rocksdb:iterator(DB, PendingCF, []),
    Nonce = get_max_nonce(Address, NonceType, Itr, rocksdb:iterator_move(Itr, first), 0),
    catch rocksdb:iterator_close(Itr),
    Nonce.

get_max_nonce(_Address, _NonceType, _Itr, {error, _Error}, Acc) ->
    Acc;
get_max_nonce(Address, NonceType, Itr, {ok, _, BinTxn}, Acc) ->
    Max =
        case nonce_info(blockchain_txn:deserialize(BinTxn)) of
            {TxnAddress, Nonce, TxnNonceType} when
                TxnAddress == Address andalso TxnNonceType == NonceType
            ->
                max(Acc, Nonce);
            _ ->
                Acc
        end,
    get_max_nonce(Address, NonceType, Itr, rocksdb:iterator_move(Itr, next), Max).

%% Calculates nonce information for a given transaction. This information %
%% includes the actor whose nonce is impacted, the nonce in the transaction and %
%% the type of nonce this is.
%%
%% NOTE: This list should include all transaction types that can be submitted to
%% the endpoint. We try to make it match what blockchain-http supports.
-spec nonce_info(supported_txn()) -> {nonce_address(), nonce(), nonce_type()}.
nonce_info(#blockchain_txn_oui_v1_pb{owner = Owner}) ->
    %% There is no nonce type for that is useful to speculate values for
    {Owner, 0, none};
nonce_info(#blockchain_txn_routing_v1_pb{nonce = Nonce}) ->
    %% oui changes could get their own oui nonce, but since there is no good actor
    %% address for it (an oui is not a public key which a lot of code relies on, we don't
    %% track it right now. We can't lean on the owner address since an owner can
    %% have multipe ouis
    {undefined, Nonce, none};
nonce_info(#blockchain_txn_vars_v1_pb{nonce = Nonce}) ->
    %% A vars transaction doesn't have a clear actor at all so we don't track it
    {undefined, Nonce, none};
nonce_info(#blockchain_txn_add_gateway_v1_pb{gateway = GatewayAddress}) ->
    %% Adding a gateway uses the gateway nonce, even though it's
    %% expected to be 0 (a gateway can only be added once)
    {GatewayAddress, 0, gateway};
nonce_info(#blockchain_txn_assert_location_v1_pb{nonce = Nonce, gateway = GatewayAddress}) ->
    %% Asserting a location uses the gatway nonce
    {GatewayAddress, Nonce, gateway};
nonce_info(#blockchain_txn_assert_location_v2_pb{nonce = Nonce, gateway = GatewayAddress}) ->
    %% Asserting a location uses the gatway nonce
    {GatewayAddress, Nonce, gateway};
nonce_info(#blockchain_txn_payment_v1_pb{nonce = Nonce, payer = Address}) ->
    {Address, Nonce, balance};
nonce_info(#blockchain_txn_payment_v2_pb{nonce = Nonce, payer = Address}) ->
    {Address, Nonce, balance};
nonce_info(#blockchain_txn_create_htlc_v1_pb{nonce = Nonce, payer = Address}) ->
    {Address, Nonce, balance};
nonce_info(#blockchain_txn_redeem_htlc_v1_pb{}) ->
    {undefined, 0, balance};
nonce_info(#blockchain_txn_price_oracle_v1_pb{public_key = Address}) ->
    {Address, 0, none};
nonce_info(#blockchain_txn_security_exchange_v1_pb{nonce = Nonce, payer = Address}) ->
    {Address, Nonce, security};
nonce_info(#blockchain_txn_transfer_hotspot_v1_pb{buyer = Buyer, buyer_nonce = Nonce}) ->
    {Buyer, Nonce, balance};
nonce_info(#blockchain_txn_transfer_hotspot_v2_pb{gateway = GatewayAddress, nonce = Nonce}) ->
    {GatewayAddress, Nonce, gateway};
nonce_info(#blockchain_txn_token_burn_v1_pb{nonce = Nonce, payer = Address}) ->
    {Address, Nonce, balance};
nonce_info(#blockchain_txn_stake_validator_v1_pb{address = Address}) ->
    {Address, 0, none};
nonce_info(#blockchain_txn_transfer_validator_stake_v1_pb{old_address = Address}) ->
    {Address, 0, none};
nonce_info(#blockchain_txn_unstake_validator_v1_pb{address = Address}) ->
    {Address, 0, none};
nonce_info(#blockchain_txn_state_channel_open_v1_pb{owner = Address, nonce = Nonce}) ->
    {Address, Nonce, dc};
nonce_info(_) ->
    undefined.

-spec load_db(Dir :: file:filename_all()) -> {ok, #state{}} | {error, any()}.
load_db(Dir) ->
    case bn_db:open_db(Dir, ["default", "pending", "status"]) of
        {error, _Reason} = Error ->
            Error;
        {ok, DB, [DefaultCF, PendingCF, StatusCF]} ->
            State = #state{
                db = DB,
                default = DefaultCF,
                pending = PendingCF,
                status = StatusCF
            },
            compact_db(State),
            {ok, State}
    end.

compact_db(#state{db = DB, default = Default, pending = PendingCF, status = StatusCF}) ->
    rocksdb:compact_range(DB, Default, undefined, undefined, []),
    rocksdb:compact_range(DB, PendingCF, undefined, undefined, []),
    rocksdb:compact_range(DB, StatusCF, undefined, undefined, []),
    ok.
