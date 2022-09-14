-module(bn_accounts).

-include("bn_jsonrpc.hrl").
-include_lib("blockchain/include/blockchain_vars.hrl").

-behavior(bn_jsonrpc_handler).

%% jsonrpc_handler
-export([handle_rpc/2]).
%% API
-export([get_speculative_nonce/3]).

%%
%% jsonrpc_handler
%%

handle_rpc(<<"account_get">>, {Param}) ->
    Chain = blockchain_worker:blockchain(),
    {Height, Ledger} = {bn_txns:follower_height(), blockchain:ledger(Chain)},
    Address = ?jsonrpc_b58_to_bin(<<"address">>, Param),
    BalanceMap =
        case get_token_version(Ledger) of
            1 -> get_balance_entry_v1(Address, Ledger);
            2 -> get_balance_entry_v2(Address, Ledger)
        end,
    {StakedBalance, CooldownBalance} = blockchain_ledger_v1:fold_validators(fun(Val, {SAcc, CAcc} = Acc) ->
            case blockchain_ledger_validator_v1:owner_address(Val) of
                Address ->
                    case blockchain_ledger_validator_v1:status(Val) of
                        staked ->
                            {SAcc + blockchain_ledger_validator_v1:stake(Val), CAcc};
                        cooldown ->
                            {SAcc, CAcc + blockchain_ledger_validator_v1:stake(Val)};
                        _ ->
                            Acc
                    end;
                _ ->
                    Acc
            end
        end, {0, 0}, Ledger),
    InfoMap =
        #{
            address => ?BIN_TO_B58(Address),
            block => Height,
            staked_balance => StakedBalance,
            cooldown_balance => CooldownBalance
        },
    maps:merge(BalanceMap, InfoMap);
handle_rpc(_, _) ->
    ?jsonrpc_error(method_not_found).

-spec get_balance_entry_v1(
    Address :: libp2p_crypto:pubkey_bin(),
    Ledger :: blockchain:ledger()
) -> map().
get_balance_entry_v1(Address, Ledger) ->
    GetBalance = fun() ->
        case blockchain_ledger_v1:find_entry(Address, Ledger) of
            {ok, Entry} ->
                #{
                    balance => blockchain_ledger_entry_v1:balance(Entry),
                    nonce => blockchain_ledger_entry_v1:nonce(Entry),
                    speculative_nonce => get_speculative_nonce(Address, balance, Ledger, 1)
                };
            _ ->
                #{
                    balance => 0,
                    nonce => 0,
                    speculative_nonce => 0
                }
        end
    end,
    GetSecurities = fun() ->
        case blockchain_ledger_v1:find_security_entry(Address, Ledger) of
            {ok, Entry} ->
                #{
                    sec_balance => blockchain_ledger_security_entry_v1:balance(Entry),
                    sec_nonce => blockchain_ledger_security_entry_v1:nonce(Entry),
                    sec_speculative_nonce => get_speculative_nonce(
                        Address,
                        security,
                        Ledger,
                        1
                    )
                };
            _ ->
                #{
                    sec_balance => 0,
                    sec_nonce => 0,
                    sec_speculative_nonce => 0
                }
        end
    end,
    GetDCs = fun() ->
        case blockchain_ledger_v1:find_dc_entry(Address, Ledger) of
            {ok, Entry} ->
                #{
                    dc_balance => blockchain_ledger_data_credits_entry_v1:balance(Entry),
                    dc_nonce => blockchain_ledger_data_credits_entry_v1:nonce(Entry)
                };
            _ ->
                #{
                    dc_balance => 0,
                    dc_nonce => 0
                }
        end
    end,
    lists:foldl(
        fun(Fun, Map) ->
            maps:merge(Map, Fun())
        end,
        #{},
        [GetBalance, GetSecurities, GetDCs]
    ).

-spec get_balance_entry_v2(
    Address :: libp2p_crypto:pubkey_bin(),
    Ledger :: blockchain:ledger()
) -> map().
get_balance_entry_v2(Address, Ledger) ->
    BalanceMap =
        case blockchain_ledger_v1:find_entry(Address, Ledger) of
            {ok, Entry} ->
                #{
                    mobile_balance => blockchain_ledger_entry_v2:balance(Entry, mobile),
                    iot_balance => blockchain_ledger_entry_v2:balance(Entry, iot),
                    sec_balance => blockchain_ledger_entry_v2:balance(Entry, hst),
                    balance => blockchain_ledger_entry_v2:balance(Entry),
                    nonce => blockchain_ledger_entry_v2:nonce(Entry),
                    speculative_nonce => get_speculative_nonce(Address, balance, Ledger, 2)
                };
            _ ->
                #{
                    mobile_balance => 0,
                    iot_balance => 0,
                    sec_balance => 0,
                    balance => 0,
                    nonce => 0,
                    speculative_nonce => 0
                }
        end,
    DCMap =
        case blockchain_ledger_v1:find_dc_entry(Address, Ledger) of
            {ok, DCEntry} ->
                #{
                    dc_balance => blockchain_ledger_data_credits_entry_v1:balance(DCEntry),
                    dc_nonce => blockchain_ledger_data_credits_entry_v1:nonce(DCEntry)
                };
            _ ->
                #{
                    dc_balance => 0,
                    dc_nonce => 0
                }
        end,
    maps:merge(BalanceMap, DCMap).

-spec get_token_version(
    Ledger :: blockchain:ledger()
) -> pos_integer().
get_token_version(Ledger) ->
    case blockchain_ledger_v1:config(?token_version, Ledger) of
        {ok, N} -> N;
        _ -> 1
    end.

-spec get_speculative_nonce(
    Address :: libp2p_crypto:pubkey_bin(),
    bn_pending_txns:nonce_type(),
    blockchain:ledger()
) -> non_neg_integer().
get_speculative_nonce(Address, NonceType, Ledger) ->
    get_speculative_nonce(Address, NonceType, Ledger, get_token_version(Ledger)).

-spec get_speculative_nonce(
    Address :: libp2p_crypto:pubkey_bin(),
    bn_pending_txns:nonce_type(),
    blockchain:ledger(),
    LedgerEntryVersion :: pos_integer()
) ->
    non_neg_integer().
get_speculative_nonce(Address, NonceType, Ledger, LedgerEntryVersion) ->
    EntryMod =
        case LedgerEntryVersion of
            1 -> blockchain_ledger_entry_v1;
            2 -> blockchain_ledger_entry_v2
        end,
    case blockchain_ledger_v1:find_entry(Address, Ledger) of
        {ok, Entry} ->
            LedgerNonce = EntryMod:nonce(Entry),
            PendingNonce = bn_pending_txns:get_max_nonce(Address, NonceType),
            max(LedgerNonce, PendingNonce);
        {error, _} ->
            0
    end.
