-module(bn_blocks).

-include("bn_jsonrpc.hrl").

-behavior(bn_jsonrpc_handler).

-export([handle_rpc/2]).

handle_rpc(<<"block_height">>, _Params) ->
    bn_txns:follower_height();
handle_rpc(<<"block_get">>, {Param}) ->
    HeightOrHash =
        case ?jsonrpc_get_param(<<"height">>, Param, false) of
            false -> ?jsonrpc_b64_to_bin(<<"hash">>, Param);
            V when is_integer(V) -> V;
            _ -> ?jsonrpc_error({invalid_params, Param})
        end,
    case blockchain:get_block(HeightOrHash, blockchain_worker:blockchain()) of
        {ok, Block} ->
            blockchain_block:to_json(Block, []);
        {error, not_found} ->
            ?jsonrpc_error({not_found, "Block not found: ~p", [Param]});
        {error, _} = Error ->
            ?jsonrpc_error(Error)
    end;
handle_rpc(_, _) ->
    ?jsonrpc_error(method_not_found).
