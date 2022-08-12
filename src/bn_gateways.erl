-module(bn_gateways).

-include("bn_jsonrpc.hrl").

-behavior(bn_jsonrpc_handler).

%% jsonrpc_handler
-export([handle_rpc/2]).

%%
%% jsonrpc_handler
%%

handle_rpc(<<"gateway_info_get">>, {Param}) ->
    Chain = blockchain_worker:blockchain(),
    {Height, Ledger} = {bn_txns:follower_height(), blockchain:ledger(Chain)},
    Address = ?jsonrpc_b58_to_bin(<<"address">>, Param),
    case blockchain_ledger_v1:find_gateway_info(Address, Ledger) of
        {ok, GWInfo} ->
            #{
                owner_address => ?BIN_TO_B58(blockchain_ledger_gateway_v2:owner_address(GWInfo)),
                location => ?MAYBE_H3(blockchain_ledger_gateway_v2:location(GWInfo)),
                alpha => blockchain_ledger_gateway_v2:alpha(GWInfo),
                beta => blockchain_ledger_gateway_v2:beta(GWInfo),
                delta => blockchain_ledger_gateway_v2:delta(GWInfo),
                last_poc_challenge => blockchain_ledger_gateway_v2:last_poc_challenge(GWInfo),
                last_poc_onion_key_hash => ?BIN_TO_B64(
                    blockchain_ledger_gateway_v2:last_poc_onion_key_hash(GWInfo)
                ),
                nonce => blockchain_ledger_gateway_v2:nonce(GWInfo),
                version => blockchain_ledger_gateway_v2:version(GWInfo),
                neighbors => blockchain_ledger_gateway_v2:neighbors(GWInfo),
                witnesses => blockchain_ledger_gateway_v2:witnesses(GWInfo),
                oui => blockchain_ledger_gateway_v2:oui(GWInfo),
                gain => blockchain_ledger_gateway_v2:gain(GWInfo),
                elevation => blockchain_ledger_gateway_v2:elevation(GWInfo),
                mode => blockchain_ledger_gateway_v2:mode(GWInfo),
                last_location_nonce => blockchain_ledger_gateway_v2:last_location_nonce(GWInfo)
            };
        {error, E} ->
            ?jsonrpc_error(
                {error, "unable to retrieve account details for ~p at height ~p due to error: ~p", [
                    ?BIN_TO_B58(Address), Height, E
                ]}
            );
        _ ->
            ?jsonrpc_error(
                {error,
                    "unable to retrieve account details for ~p at height ~p due to unknown error.",
                    [?BIN_TO_B58(Address), Height]}
            )
    end;
handle_rpc(_, _) ->
    ?jsonrpc_error(method_not_found).
