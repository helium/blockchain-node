-module(bn_blocks).

-include("bn_jsonrpc.hrl").
-behavior(bn_jsonrpc_handler).

-export([handle_rpc/2]).

handle_rpc(<<"block_height">>, _Params) ->
    bn_txns:follower_height();

handle_rpc(<<"block_get">>, [Param]) ->
    HeightOrHash = case Param of
                       V when is_binary(V) -> ?B64_TO_BIN(V);
                       V when is_integer(V) -> V;
                       _ -> throw({invalid_params, Param})
                   end,
    case blockchain:get_block(HeightOrHash, blockchain_worker:blockchain()) of
        {ok, Block} ->
            blockchain_block:to_json(Block, []);
        {error, _} ->
            throw({invalid_params, {HeightOrHash}})
    end;

handle_rpc(_, _) ->
    throw(method_not_found).
