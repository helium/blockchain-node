-module(bn_htlc).

-include("bn_jsonrpc.hrl").
-behavior(bn_jsonrpc_handler).

-export([handle_rpc/2]).

%%
%% jsonrpc_handler
%%

handle_rpc(<<"htlc_get">>, {Param}) ->
    Address = ?jsonrpc_b58_to_bin(<<"address">>, Param),
    Chain = blockchain_worker:blockchain(),
    Ledger = blockchain:ledger(Chain),
    case blockchain_ledger_v1:find_htlc(Address, Ledger) of
        {ok, HTLC} ->
            #{
                payer => ?BIN_TO_B58(blockchain_ledger_htlc_v1:payer(HTLC)),
                payee => ?BIN_TO_B58(blockchain_ledger_htlc_v1:payee(HTLC)),
                amount => blockchain_ledger_htlc_v1:balance(HTLC),
                hashlock => ?BIN_TO_B64(blockchain_ledger_htlc_v1:hashlock(HTLC)),
                timelock => blockchain_ledger_htlc_v1:timelock(HTLC)
            };
        {error, not_found} ->
            case blockchain:get_htlc_receipt(Address, Chain) of
                {ok, HTLCReceipt} ->
                    blockchain_htlc_receipt:to_json(HTLCReceipt, []);
                {error, _}=Error ->
                    ?jsonrpc_error(Error)
            end;
        {error, _}=Error ->
            ?jsonrpc_error(Error)
    end;
handle_rpc(_, _) ->
    ?jsonrpc_error(method_not_found).
