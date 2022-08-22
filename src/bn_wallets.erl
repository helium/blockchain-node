-module(bn_wallets).

-include("bn_jsonrpc.hrl").

-behavior(bn_jsonrpc_handler).
-behavior(gen_server).

%% gen_server
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
%% jsonrpc_handler
-export([handle_rpc/2]).

-define(DB_FILE, "wallets.db").
-define(SERVER, ?MODULE).
-define(KEY_TIMEOUT, 60000).
-define(UNLOCK_TIMEOUT, 30000).

-record(state, {
    dir :: file:filename_all(),
    db :: rocksdb:db_handle(),
    default :: rocksdb:cf_handle(),
    wallets :: rocksdb:cf_handle(),
    keys = #{} :: #{libp2p_crypto:pubkey_bin() => libp2p_crypto:key_map()}
}).

-spec unlock(libp2p_crypto:pubkey_bin(), binary()) -> ok | {error, term()}.
unlock(Address, Password) ->
    gen_server:call(?SERVER, {unlock, Address, Password}, ?UNLOCK_TIMEOUT).

-spec sign(libp2p_crypto:pubkey_bin(), blockchain_txn:txn()) ->
    {ok, blockchain_txn:txn()} | {error, term()}.
sign(Address, Txn) ->
    gen_server:call(?SERVER, {sign, Address, Txn}).

-spec lock(libp2p_crypto:pubkey_bin()) -> ok.
lock(Address) ->
    gen_server:call(?SERVER, {lock, Address}).

-spec is_locked(libp2p_crypto:pubkey_bin()) -> boolean().
is_locked(Address) ->
    gen_server:call(?SERVER, {is_locked, Address}).

restore(Path, BackupID) ->
    gen_server:call(?SERVER, {restore, Path, BackupID}).

%%
%% gen_server
%%
start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

init(Args) ->
    Dir = filename:join(proplists:get_value(base_dir, Args, "data"), ?DB_FILE),
    case load_db(Dir) of
        {ok, State} ->
            persistent_term:put(?MODULE, State),
            {ok, State};
        Error ->
            Error
    end.

handle_call({unlock, Address, Password}, _From, State) ->
    case maps:is_key(Address, State#state.keys) of
        true ->
            {reply, ok, State};
        false ->
            case get_wallet(Address, State) of
                {error, Error} ->
                    {reply, {error, Error}, State};
                {ok, Wallet} ->
                    case wallet:decrypt(Password, Wallet) of
                        {error, Error} ->
                            {reply, {error, Error}, State};
                        {ok, KeyMap} ->
                            timer:send_after(?KEY_TIMEOUT, self(), {key_timeout, Address}),
                            {reply, ok, State#state{
                                keys = maps:put(Address, KeyMap, State#state.keys)
                            }}
                    end
            end
    end;
handle_call({lock, Address}, _From, State) ->
    {reply, ok, State#state{keys = maps:remove(Address, State#state.keys)}};
handle_call({is_locked, Address}, _From, State) ->
    {reply, not maps:is_key(Address, State#state.keys), State};
handle_call({sign, Address, Txn}, _From, State) ->
    case maps:get(Address, State#state.keys, false) of
        false ->
            {reply, {error, not_found}, State};
        #{secret := PrivKey} ->
            SigFun = libp2p_crypto:mk_sig_fun(PrivKey),
            {reply, {ok, blockchain_txn:sign(Txn, SigFun)}, State}
    end;
handle_call({restore, Path, BackupID}, _From, State) ->
    {ok, Engine} = rocksdb:open_backup_engine(Path),
    case rocksdb:verify_backup(Engine, BackupID) of
        {error, Error} ->
            {reply, {error, Error}, State};
        ok ->
            rocksdb:close(State#state.db),
            case rocksdb:restore_db_from_backup(Engine, BackupID, State#state.dir) of
                ok ->
                    case load_db(State#state.dir) of
                        {ok, NewState} ->
                            persistent_term:put(?MODULE, NewState),
                            {reply, ok, NewState};
                        Error ->
                            {reply, Error, State}
                    end
            end
    end;
handle_call(Request, _From, State) ->
    lager:notice("Unhandled call ~p", [Request]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    lager:notice("Unhandled cast ~p", [Msg]),
    {noreply, State}.

handle_info({key_timeout, Address}, State) ->
    {noreply, State#state{keys = maps:remove(Address, State#state.keys)}};
handle_info(Info, State) ->
    lager:notice("Unhandled info ~p", [Info]),
    {noreply, State}.

terminate(_Reason, #state{db = DB}) ->
    rocksdb:close(DB).

%%
%% jsonrpc_handler
%%

handle_rpc(<<"wallet_create">>, {Param}) ->
    KeyMap = libp2p_crypto:generate_keys(ed25519),
    Password =
        case ?jsonrpc_get_param(<<"password">>, Param) of
            V when is_binary(V) andalso byte_size(V) > 0 -> V;
            _ -> ?jsonrpc_error(invalid_params)
        end,
    {ok, State} = get_state(),
    {ok, Wallet} = wallet:encrypt(KeyMap, Password),
    ok = save_wallet(Wallet, State),
    ?BIN_TO_B58(wallet:pubkey_bin(Wallet));
handle_rpc(<<"wallet_delete">>, {Param}) ->
    Address = ?jsonrpc_b58_to_bin(<<"address">>, Param),
    {ok, State} = get_state(),
    case delete_wallet(Address, State) of
        {error, _} = Error ->
            ?jsonrpc_error(Error);
        ok ->
            true
    end;
handle_rpc(<<"wallet_list">>, _Params) ->
    {ok, State} = get_state(),
    [?BIN_TO_B58(Addr) || Addr <- get_wallet_list(State)];
handle_rpc(<<"wallet_unlock">>, {Param}) ->
    Address = ?jsonrpc_b58_to_bin(<<"address">>, Param),
    Password = ?jsonrpc_get_param(<<"password">>, Param),
    case unlock(Address, Password) of
        {error, not_found} ->
            ?jsonrpc_error({not_found, "Wallet not found"});
        {error, decrypt} ->
            ?jsonrpc_error(invalid_password);
        ok ->
            true
    end;
handle_rpc(<<"wallet_lock">>, {Param}) ->
    Address = ?jsonrpc_b58_to_bin(<<"address">>, Param),
    ok = lock(Address),
    true;
handle_rpc(<<"wallet_is_locked">>, {Param}) ->
    Address = ?jsonrpc_b58_to_bin(<<"address">>, Param),
    is_locked(Address);
handle_rpc(<<"wallet_pay">>, {Param}) ->
    Payer = ?jsonrpc_b58_to_bin(<<"address">>, Param),
    Payee = ?jsonrpc_b58_to_bin(<<"payee">>, Param),
    Amount = ?jsonrpc_get_param(<<"bones">>, Param, undefined),
    Max = ?jsonrpc_get_param(<<"max">>, Param, false),
    Chain = blockchain_worker:blockchain(),
    Nonce = jsonrpc_nonce_param(Param, Payer, balance, Chain),

    {ok, Txn} = mk_payment_txn_v2(Payer, [{Payee, Amount, Max}], Nonce, Chain),
    case sign(Payer, Txn) of
        {ok, SignedTxn} ->
            {ok, _} = bn_pending_txns:submit_txn(SignedTxn),
            blockchain_txn:to_json(SignedTxn, []);
        {error, not_found} ->
            ?jsonrpc_error({not_found, "Wallet is locked"})
    end;
handle_rpc(<<"wallet_pay_multi">>, {Param}) ->
    Payer = ?jsonrpc_b58_to_bin(<<"address">>, Param),
    Payments =
        case ?jsonrpc_get_param(<<"payments">>, Param, false) of
            L when is_list(L) andalso length(L) > 0 ->
                lists:map(
                    fun(Entry) ->
                        Payee = ?jsonrpc_b58_to_bin(<<"payee">>, Entry),
                        Amount = ?jsonrpc_get_param(<<"bones">>, Entry, undefined),
                        Max = ?jsonrpc_get_param(<<"max">>, Entry, false),
                        {Payee, Amount, Max}
                    end,
                    L
                );
            _ ->
                ?jsonrpc_error({invalid_params, "Missing or empty payment list"})
        end,
    Chain = blockchain_worker:blockchain(),
    Nonce = jsonrpc_nonce_param(Param, Payer, balance, Chain),

    {ok, Txn} = mk_payment_txn_v2(Payer, Payments, Nonce, Chain),
    case sign(Payer, Txn) of
        {ok, SignedTxn} ->
            {ok, _} = bn_pending_txns:submit_txn(SignedTxn),
            blockchain_txn:to_json(SignedTxn, []);
        {error, not_found} ->
            ?jsonrpc_error({not_found, "Wallet is locked"})
    end;
handle_rpc(<<"wallet_import">>, {Param}) ->
    Password = ?jsonrpc_get_param(<<"password">>, Param),
    Path = ?jsonrpc_get_param(<<"path">>, Param),
    {ok, State} = get_state(),
    case file:read_file(Path) of
        {error, enoent} ->
            ?jsonrpc_error({not_found, "Path not found"});
        {error, _} = Error ->
            ?jsonrpc_error(Error);
        {ok, FileBin} ->
            case wallet:from_binary(FileBin) of
                {error, _} = Error ->
                    ?jsonrpc_error(Error);
                {ok, Wallet} ->
                    case wallet:decrypt(Password, Wallet) of
                        {error, decrypt} ->
                            ?jsonrpc_error(invalid_password);
                        {ok, _} ->
                            ok = save_wallet(Wallet, State),
                            ?BIN_TO_B58(wallet:pubkey_bin(Wallet))
                    end
            end
    end;
handle_rpc(<<"wallet_export">>, {Param}) ->
    Address = ?jsonrpc_b58_to_bin(<<"address">>, Param),
    Path = ?jsonrpc_get_param(<<"path">>, Param),
    {ok, State} = get_state(),
    case get_wallet(Address, State) of
        {error, not_found} ->
            ?jsonrpc_error({not_found, "Wallet not found"});
        {ok, Wallet} ->
            WalletBin = wallet:to_binary(Wallet),
            case file:write_file(Path, WalletBin) of
                ok -> true;
                {error, _} = Error -> ?jsonrpc_error(Error)
            end
    end;
handle_rpc(<<"wallet_backup_list">>, {Param}) ->
    Path = ?jsonrpc_get_param(<<"path">>, Param),
    {ok, Engine} = rocksdb:open_backup_engine(binary_to_list(Path)),
    {ok, Info} = rocksdb:get_backup_info(Engine),
    Info;
handle_rpc(<<"wallet_backup_create">>, {Param}) ->
    Path = ?jsonrpc_get_param(<<"path">>, Param),
    NumBackupToKeep = ?jsonrpc_get_param(<<"max_backups">>, Param),
    {ok, Engine} = rocksdb:open_backup_engine(binary_to_list(Path)),
    {ok, #state{db = DB}} = get_state(),
    ok = rocksdb:create_new_backup(Engine, DB),
    ok = rocksdb:purge_old_backup(Engine, NumBackupToKeep),
    {ok, Info} = rocksdb:get_backup_info(Engine),
    LastBackup = hd(Info),
    LastBackup;
handle_rpc(<<"wallet_backup_delete">>, {Param}) ->
    Path = ?jsonrpc_get_param(<<"path">>, Param),
    BackupID = ?jsonrpc_get_param(<<"backup_id">>, Param),
    {ok, Engine} = rocksdb:open_backup_engine(binary_to_list(Path)),
    case rocksdb:delete_backup(Engine, BackupID) of
        ok ->
            true;
        {error, not_found} ->
            ?jsonrpc_error({not_found, "Backup not found: ~p", [BackupID]});
        {error, _} = Error ->
            ?jsonrpc_error(Error)
    end;
handle_rpc(<<"wallet_backup_restore">>, {Param}) ->
    Path = ?jsonrpc_get_param(<<"path">>, Param),
    BackupID = ?jsonrpc_get_param(<<"backup_id">>, Param),
    case restore(binary_to_list(Path), BackupID) of
        ok ->
            true;
        {error, not_found} ->
            ?jsonrpc_error({not_found, "Backup not found: ~p", [BackupID]});
        {error, _} = Error ->
            ?jsonrpc_error(Error)
    end;
handle_rpc(_, _) ->
    ?jsonrpc_error(method_not_found).

%%
%% Internal
%%

%% Gets a nonce from a given jsonrpc parameter list. If not present it gets the
%% speculative nonce for the given account address and adds one to construct a new
%% nonce.
-spec jsonrpc_nonce_param(
    [term()],
    libp2p_crypto:pubkey_bin(),
    bn_pending_txns:nonce_type(),
    blockchain:chain()
) ->
    non_neg_integer().
jsonrpc_nonce_param(Param, Address, NonceType, Chain) ->
    case ?jsonrpc_get_param(<<"nonce">>, Param, false) of
        false ->
            bn_accounts:get_speculative_nonce(
                Address,
                NonceType,
                blockchain:ledger(Chain)
            ) + 1;
        V when is_integer(V) ->
            V;
        _ ->
            ?jsonrpc_error({invalid_params, Param})
    end.

-spec mk_payment_txn_v2(
    Payer :: libp2p_crypto:pubkey_bin(),
    [{Payee :: libp2p_crypto:pubkey_bin(), Bones :: pos_integer() | undefined, Max :: boolean()}],
    Nonce :: non_neg_integer(),
    Chain :: blockchain:blockchain()
) ->
    {ok, blockchain_txn:txn()} | {error, term()}.
mk_payment_txn_v2(Payer, PaymentList, Nonce, Chain) ->
    Payments = [mk_payment(Payee, Bones, Max) || {Payee, Bones, Max} <- PaymentList],
    Txn = blockchain_txn_payment_v2:new(Payer, Payments, Nonce),
    TxnFee = blockchain_txn_payment_v2:calculate_fee(Txn, Chain),
    {ok, blockchain_txn_payment_v2:fee(Txn, TxnFee)}.

mk_payment(Payee, undefined, true) ->
    blockchain_payment_v2:new(Payee, max);
mk_payment(Payee, Bones, false) ->
    blockchain_payment_v2:new(Payee, Bones).

get_state() ->
    case persistent_term:get(?MODULE, false) of
        false ->
            {error, {no_database, ?MODULE}};
        State ->
            {ok, State}
    end.

-spec get_wallet(libp2p_crypto:pubkey_bin(), #state{}) ->
    {ok, wallet:wallet()} | {error, term()}.
get_wallet(Address, #state{db = DB, wallets = WalletCF}) ->
    case rocksdb:get(DB, WalletCF, Address, []) of
        not_found ->
            {error, not_found};
        {ok, BinWallet} ->
            wallet:from_binary(BinWallet);
        Error ->
            Error
    end.

get_wallet_list(#state{db = DB, wallets = WalletCF}) ->
    {ok, Itr} = rocksdb:iterator(DB, WalletCF, []),
    Wallets = get_wallet_list(Itr, rocksdb:iterator_move(Itr, first), []),
    catch rocksdb:iterator_close(Itr),
    Wallets.

get_wallet_list(_Itr, {error, _Error}, Acc) ->
    lists:reverse(Acc);
get_wallet_list(Itr, {ok, Addr, _}, Acc) ->
    get_wallet_list(Itr, rocksdb:iterator_move(Itr, next), [Addr | Acc]).

-spec save_wallet(wallet:wallet(), #state{}) -> ok | {error, term()}.
save_wallet(Wallet, #state{db = DB, wallets = WalletCF}) ->
    PubKeyBin = wallet:pubkey_bin(Wallet),
    WalletBin = wallet:to_binary(Wallet),
    rocksdb:put(DB, WalletCF, PubKeyBin, WalletBin, [{sync, true}]).

-spec delete_wallet(Address :: libp2p_crypto:pubkey_bin(), #state{}) ->
    ok | {error, term()}.
delete_wallet(Address, #state{db = DB, wallets = WalletCF}) ->
    rocksdb:delete(DB, WalletCF, Address, [{sync, true}]).

-spec load_db(file:filename_all()) -> {ok, #state{}} | {error, any()}.
load_db(Dir) ->
    case bn_db:open_db(Dir, ["default", "wallets"]) of
        {error, _Reason} = Error ->
            Error;
        {ok, DB, [DefaultCF, WalletCF]} ->
            State = #state{
                dir = Dir,
                db = DB,
                default = DefaultCF,
                wallets = WalletCF
            },
            compact_db(State),
            {ok, State}
    end.

compact_db(#state{db = DB, default = Default, wallets = WalletCF}) ->
    rocksdb:compact_range(DB, Default, undefined, undefined, []),
    rocksdb:compact_range(DB, WalletCF, undefined, undefined, []),
    ok.
