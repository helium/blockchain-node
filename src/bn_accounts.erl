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
    case ?jsonrpc_get_param(<<"height">>, Param, false) of
        false ->
            Ledger = blockchain:ledger(Chain);
        Height ->
            Ledger = blockchain:ledger_at(Height, Chain)
    end,
    Address = ?jsonrpc_b58_to_bin(<<"address">>, Param),
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
            block => bn_txns:follower_height()
        },
        [GetBalance, GetSecurities, GetDCs]
    );

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
