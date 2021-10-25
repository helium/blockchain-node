-module(bn_accounts).

-include("bn_jsonrpc.hrl").

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
    {Height, Ledger} =
        case ?jsonrpc_get_param(<<"height">>, Param, false) of
            false ->
                {bn_txns:follower_height(), blockchain:ledger(Chain)};
            V ->
                case blockchain:ledger_at(V, Chain) of
                    {ok, OldLedger} ->
                        {V, OldLedger};
                    {error, height_too_old} ->
                        case application:get_env(blockchain, store_historic_balances, false) of
                            true ->
                                {V, {error, height_too_old}};
                            false ->
                                {V, ?jsonrpc_error(
                                    {error, "height ~p is too old to get historic account data", [V]}
                                )}
                        end;
                    {error, _} = Error ->
                        {V, ?jsonrpc_error(Error)}
                end
        end,
    Address = ?jsonrpc_b58_to_bin(<<"address">>, Param),
    case Ledger of
        {error, height_too_old} ->
            case bn_balances:get_historic_entry(Address, Height) of
                {ok, {EntryBin, DCEntryBin, SecurityEntryBin}} ->
                    Entry = blockchain_ledger_entry_v1:deserialize(EntryBin),
                    DCEntry = blockchain_ledger_data_credits_entry_v1:deserialize(DCEntryBin),
                    SecurityEntry = blockchain_ledger_security_entry_v1:deserialize(SecurityEntryBin),
                    #{
                        address => ?BIN_TO_B58(Address),
                        block => Height,
                        balance => blockchain_ledger_entry_v1:balance(Entry),
                        nonce => blockchain_ledger_entry_v1:nonce(Entry),
                        sec_balance => blockchain_ledger_security_entry_v1:balance(SecurityEntry),
                        sec_nonce => blockchain_ledger_security_entry_v1:nonce(SecurityEntry),
                        dc_balance => blockchain_ledger_data_credits_entry_v1:balance(DCEntry),
                        dc_nonce => blockchain_ledger_data_credits_entry_v1:nonce(DCEntry)
                    };
                {error, _} ->
                    ?jsonrpc_error({error, "unable to retrieve account details for ~p at height ~p", [?BIN_TO_B58(Address), Height]})
            end;
        _ ->
            GetBalance = fun() ->
                case blockchain_ledger_v1:find_entry(Address, Ledger) of
                    {ok, Entry} ->
                        #{
                            balance => blockchain_ledger_entry_v1:balance(Entry),
                            nonce => blockchain_ledger_entry_v1:nonce(Entry),
                            speculative_nonce => get_speculative_nonce(Address, balance, Ledger)
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
                                Ledger
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
                #{
                    address => ?BIN_TO_B58(Address),
                    block => Height
                },
                [GetBalance, GetSecurities, GetDCs]
            )
    end;
handle_rpc(_, _) ->
    ?jsonrpc_error(method_not_found).

-spec get_speculative_nonce(
    Address :: libp2p_crypto:pubkey_bin(),
    bn_pending_txns:nonce_type(),
    blockchain:ledger()
) ->
    non_neg_integer().
get_speculative_nonce(Address, NonceType, Ledger) ->
    case blockchain_ledger_v1:find_entry(Address, Ledger) of
        {ok, Entry} ->
            LedgerNonce = blockchain_ledger_entry_v1:nonce(Entry),
            PendingNonce = bn_pending_txns:get_max_nonce(Address, NonceType),
            max(LedgerNonce, PendingNonce);
        {error, _} ->
            0
    end.