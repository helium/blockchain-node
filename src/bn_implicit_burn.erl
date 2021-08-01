-module(bn_implicit_burn).

-include("bn_jsonrpc.hrl").
-behavior(bn_jsonrpc_handler).

-export([handle_rpc/2]).

%%
%% jsonrpc_handler
%%

handle_rpc(<<"implicit_burn_get">>, {Param}) ->
    Hash = ?jsonrpc_b64_to_bin(<<"hash">>, Param),
    Chain = blockchain_worker:blockchain(),
    case blockchain:get_implicit_burn(Hash, Chain) of
        {ok, ImplicitBurn} ->
            blockchain_implicit_burn:to_json(ImplicitBurn, []);
        {error, not_found} ->
            ?jsonrpc_error({not_found, "Implicit burn not found for transaction hash: ~p", [Param]});
        {error, _}=Error ->
            ?jsonrpc_error(Error)
    end;
handle_rpc(_, _) ->
    ?jsonrpc_error(method_not_found).
