%%%-------------------------------------------------------------------
%%
%% Handler module for the follower service's streaming API/RPC
%%
%%%-------------------------------------------------------------------
-module(helium_follower_service).

-behaviour(helium_follower_follower_bhvr).

-include("grpc/autogen/server/follower_pb.hrl").
-include_lib("blockchain/include/blockchain.hrl").
-include_lib("blockchain/include/blockchain_vars.hrl").

-export([
         txn_stream/2,
         find_gateway/2,
         subnetwork_last_reward_height/2,
         active_gateways/2
        ]).

-export([init/2, handle_info/2]).

-type handler_state() :: #{
    chain => blockchain:blockchain(),
    streaming_initialized => boolean(),
    txn_types => [atom()]
}.

-export_type([handler_state/0]).

-define(GW_STREAM_BATCH_SIZE, 5000).

%% -------------------------------------------------------------------
%% helium_follower_bhvr callback functions
%% -------------------------------------------------------------------
-spec txn_stream(follower_pb:follower_txn_stream_req_v1_pb(), grpcbox_stream:t()) ->
    {ok, grpcbox_stream:t()} | grpcbox_stream:grpc_error_response().
txn_stream(#follower_txn_stream_req_v1_pb{} = Msg, StreamState) ->
    #{chain := Chain, streaming_initialized := StreamInitialized} =
        grpcbox_stream:stream_handler_state(StreamState),
    txn_stream(Chain, StreamInitialized, Msg, StreamState);
txn_stream(_Msg, StreamState) ->
    lager:warning("unhandled grpc msg ~p", [_Msg]),
    {ok, StreamState}.

-spec find_gateway(ctx:ctx(), follower_pb:follower_gateway_req_v1_pb()) ->
    {ok, follower_pb:follower_gateway_resp_v1_pb(), ctx:ctx()}
    | grpcbox_stream:grpc_error_response().
find_gateway(Ctx, Req) ->
    Chain = blockchain_worker:cached_blockchain(),
    find_gateway(Chain, Ctx, Req).

-spec subnetwork_last_reward_height(
    ctx:ctx(), follower_pb:follower_subnetwork_last_reward_height_req_v1_pb()
) ->
    {ok, follower_pb:follower_subnetwork_last_reward_height_resp_v1_pb(), ctx:ctx()}
    | grpcbox_stream:grpc_error_response().
subnetwork_last_reward_height(Ctx, Req) ->
    Chain = blockchain_worker:cached_blockchain(),
    subnetwork_last_reward_height(Chain, Ctx, Req).

-spec active_gateways(follower_pb:follower_gateway_stream_req_v1_pb(), grpcbox_stream:t()) ->
        {ok, grpcbox_stream:t()} | grpcbox_stream:grpc_error_response().
active_gateways(#follower_gateway_stream_req_v1_pb{batch_size = BatchSize} = _Msg, StreamState) when BatchSize =< ?GW_STREAM_BATCH_SIZE ->
    #{chain := Chain} =  grpcbox_stream:stream_handler_state(StreamState),
    case Chain of
        undefined ->
            lager:debug("chain not ready, returning error response for msg ~p", [_Msg]),
            {grpc_error, {grpcbox_stream:code_to_status(14), <<"temporarily unavailable">>}};
        _ ->
            Ledger = blockchain:ledger(Chain),
            {ok, Height} = blockchain_ledger_v1:current_height(Ledger),
            {NumGateways, FinalGws, StreamState1} =
                blockchain_ledger_v1:cf_fold(
                    active_gateways,
                    fun({Addr, BinGw}, {CountAcc, GwAcc, StreamAcc}) ->
                        Gw = blockchain_ledger_gateway_v2:deserialize(BinGw),
                        Loc = blockchain_ledger_gateway_v2:location(Gw),
                        case Loc /= undefined of
                            true ->
                                Region = case blockchain_region_v1:h3_to_region(Loc, Ledger) of
                                             {ok, R} -> normalize_region(R);
                                             _ -> undefined
                                         end,
                                RegionParams =
                                    case region_params_for_region(Region, Ledger) of
                                        {ok, Params} -> Params;
                                        {error, _} -> []
                                    end,
                                GwResp = #follower_gateway_resp_v1_pb{
                                          height = Height,
                                          result = {info, #gateway_info_pb{
                                              address = Addr,
                                              location = h3:to_string(Loc),
                                              owner = blockchain_ledger_gateway_v2:owner_address(Gw),
                                              staking_mode = blockchain_ledger_gateway_v2:mode(Gw),
                                              gain = blockchain_ledger_gateway_v2:gain(Gw),
                                              region = Region,
                                              region_params = #blockchain_region_params_v1_pb{
                                                    region_params = RegionParams
                                                }
                                      }}},
                                GwAcc1 = [GwResp | GwAcc],
                                RespLen = length(GwAcc1),
                                case RespLen >= BatchSize of
                                    true ->
                                        Resp = #follower_gateway_stream_resp_v1_pb{ gateways = GwAcc1 },
                                        StreamAcc1 = grpcbox_stream:send(false, Resp, StreamAcc),
                                        {CountAcc + RespLen, [], StreamAcc1};
                                    _ ->
                                        {CountAcc, GwAcc1, StreamAcc}
                                end;
                            _ -> {CountAcc, GwAcc, StreamAcc}
                        end
                    end, {0, [], StreamState}, Ledger),

            FinalGwLen = length(FinalGws),
            {FinalGwCount, StreamState3} =
                case FinalGwLen > 0 of
                    true ->
                        Resp = #follower_gateway_stream_resp_v1_pb{ gateways = FinalGws },
                        StreamState2 = grpcbox_stream:send(false, Resp, StreamState1),
                        {NumGateways + FinalGwLen, StreamState2};
                    _ ->
                        {NumGateways, StreamState1}
                end,

            StreamState4 = grpcbox_stream:update_trailers([{<<"num_gateways">>, integer_to_binary(FinalGwCount)}], StreamState3),
            {stop, StreamState4}
    end;
active_gateways(#follower_gateway_stream_req_v1_pb{batch_size = BatchSize} = _Msg, _StreamState) ->
    lager:info("Requested batch size exceeds maximum allowed batch count: ~p", [BatchSize]),
    {grpc_error, {grpcbox_stream:code_to_status(3), <<"maximum batch size exceeded">>}};
active_gateways(_Msg, StreamState) ->
    lager:warning("unhandled grpc msg ~p", [_Msg]),
    {ok, StreamState}.

-spec init(atom(), grpcbox_stream:t()) -> grpcbox_stream:t().
init(_RPC, StreamState) ->
    lager:debug("handler init, stream state ~p", [StreamState]),
    Chain = blockchain_worker:blockchain(),
    _NewStreamState = grpcbox_stream:stream_handler_state(
                         StreamState,
                         #{chain => Chain, streaming_initialized => false}
                     ).

handle_info({blockchain_event, {add_block, BlockHash, _Sync, _Ledger}}, StreamState) ->
    #{chain := Chain, txn_types := TxnTypes} = grpcbox_stream:stream_handler_state(StreamState),
    case blockchain:get_block(BlockHash, Chain) of
        {ok, Block} ->
            Height = blockchain_block:height(Block),
            Timestamp = blockchain_block:time(Block),
            SortedTxns = filter_hash_sort_txns(Block, TxnTypes),
            _NewStreamState = send_txn_sequence(SortedTxns, Height, Timestamp, StreamState);
        _ ->
            lager:error("failed to find block with hash: ~p", [BlockHash]),
            StreamState
    end;
handle_info(_Msg, StreamState) ->
    lager:warning("unhandled info msg: ~p", [_Msg]),
    StreamState.

%% -------------------------------------------------------------------
%% internal and callback breakdown functions
%% -------------------------------------------------------------------
-spec txn_stream(
    blockchain:blockchain() | undefined,
    boolean(),
    follower_pb:follower_txn_stream_req_v1_pb(),
    grpcbox_stream:t()
) ->
    {ok, grpcbox_stream:t()} | grpcbox_stream:grpc_error_response().
txn_stream(undefined = _Chain, _StreamInitialized, _Msg, _StreamState) ->
    lager:debug("chain not ready, returning error response for msg ~p", [_Msg]),
    {grpc_error, {grpcbox_stream:code_to_status(14), <<"temporarily unavailable">>}};
txn_stream(_Chain, true = _StreamInitialized, _Msg, StreamState) ->
    {ok, StreamState};
txn_stream(
    Chain,
    false = _StreamInitialized,
    #follower_txn_stream_req_v1_pb{height = Height, txn_hash = Hash, txn_types = TxnTypes0} = _Msg,
    StreamState
) ->
    lager:debug("subscribing client to txn stream with msg ~p", [_Msg]),
    ok = blockchain_event:add_handler(self()),
    case validate_txn_filters(TxnTypes0) of
        {error, invalid_filters} ->
            {grpc_error, {grpcbox_stream:code_to_status(3), <<"invalid txn filter">>}};
        {ok, TxnTypes} ->
            case process_past_blocks(Height, Hash, TxnTypes, Chain, StreamState) of
                {ok, StreamState1} ->
                    HandlerState = grpcbox_stream:stream_handler_state(StreamState1),
                    StreamState2 = grpcbox_stream:stream_handler_state(
                        StreamState1,
                        HandlerState#{streaming_initialized => true, txn_types => TxnTypes}
                    ),
                    {ok, StreamState2};
                {error, invalid_req_params} ->
                    {grpc_error, {
                        grpcbox_stream:code_to_status(3),
                        <<"invalid starting height, txn hash, or filter">>
                    }};
                {error, _} ->
                    {grpc_error, {
                        grpcbox_stream:code_to_status(5), <<"requested block not found">>
                    }}
            end
    end.

-spec find_gateway(
    blockchain:chain() | undefined, ctx:ctx(), follower_pb:follower_gateway_req_v1_pb()
) ->
    {ok, follower_pb:follower_gateway_resp_v1_pb(), ctx:ctx()}
    | grpcbox_stream:grpc_error_response().
find_gateway(undefined = _Chain, _Ctx, _Req) ->
    lager:debug("chain not ready, returning error response for msg ~p", [_Req]),
    {grpc_error, {grpcbox_stream:code_to_status(14), <<"temporarily unavailable">>}};
find_gateway(Chain, Ctx, Req) ->
    Ledger = blockchain:ledger(Chain),
    PubKeyBin = Req#follower_gateway_req_v1_pb.address,
    {ok, Height} = blockchain_ledger_v1:current_height(Ledger),
    case blockchain_ledger_v1:find_gateway_info(PubKeyBin, Ledger) of
        {ok, GwInfo} ->
            {Location, Region} =
                case blockchain_ledger_gateway_v2:location(GwInfo) of
                    undefined -> {<<>>, undefined};
                    H3 ->
                        case blockchain_region_v1:h3_to_region(H3, Ledger) of
                            {ok, R} -> {h3:to_string(H3), normalize_region(R)};
                            _ -> {h3:to_string(H3), undefined}
                        end
                end,
            RegionParams =
                case region_params_for_region(Region, Ledger) of
                    {ok, Params} -> Params;
                    {error, _} -> []
                end,
            {ok,
                #follower_gateway_resp_v1_pb{
                    height = Height,
                    result = {info, #gateway_info_pb{
                        address = PubKeyBin,
                        location = Location,
                        owner = blockchain_ledger_gateway_v2:owner_address(GwInfo),
                        staking_mode = blockchain_ledger_gateway_v2:mode(GwInfo),
                        gain = blockchain_ledger_gateway_v2:gain(GwInfo),
                        region = Region,
                        region_params = #blockchain_region_params_v1_pb{
                                region_params = RegionParams
                            }
                    }}
                },
                Ctx};
        _ ->
            ErrorResult = {error, #follower_error_pb{type = {not_found, #gateway_not_found_pb{address = PubKeyBin}}}},
            {ok, #follower_gateway_resp_v1_pb{height = Height, result = ErrorResult}, Ctx}
    end.

-spec subnetwork_last_reward_height(
    blockchain:chain() | undefined,
    ctx:ctx(),
    follower_pb:follower_subnetwork_last_reward_height_req_v1_pb()
) ->
    {ok, follower_pb:follower_subnetwork_last_reward_height_resp_v1_pb(), ctx:ctx()}
    | grpcbox_stream:grpc_error_response().
subnetwork_last_reward_height(undefined = _Chain, _Ctx, _Req) ->
    lager:debug("chain not ready, returning error response for msg ~p", [_Req]),
    {grpc_error, {grpcbox_stream:code_to_status(14), <<"temporarily unavailable">>}};
subnetwork_last_reward_height(Chain, Ctx, Req) ->
    Ledger = blockchain:ledger(Chain),
    TokenType = Req#follower_subnetwork_last_reward_height_req_v1_pb.token_type,
    {ok, CurrentHeight} = blockchain_ledger_v1:current_height(Ledger),
    case blockchain_ledger_v1:find_subnetwork_v1(TokenType, Ledger) of
        {ok, SubnetworkLedger} ->
            LastRewardHt = blockchain_ledger_subnetwork_v1:last_rewarded_block(SubnetworkLedger),
            {ok, #follower_subnetwork_last_reward_height_resp_v1_pb{height = CurrentHeight, reward_height = LastRewardHt}, Ctx};
        _ -> {grpc_error, {grpcbox_stream:code_to_status(3), <<"unable to get retrieve subnetwork for requested token">>}}
    end.

-spec process_past_blocks(Height      :: pos_integer(),
                          TxnHash     :: binary(),
                          TxnTypes    :: [atom()],
                          Chain       :: blockchain:blockchain(),
                          StreamState :: grpcbox_stream:t()) -> {ok, grpcbox_stream:t()} | {error, term()}.
process_past_blocks(Height, TxnHash, TxnTypes, Chain, StreamState) when is_integer(Height) andalso Height > 0 ->
    {ok, #block_info_v2{height = HeadHeight}} = blockchain:head_block_info(Chain),
    case Height > HeadHeight of
        %% requested a future block; nothing to do but wait
        true -> {ok, StreamState};
        false ->
            case blockchain:get_block(Height, Chain) of
                {ok, SubscribeBlock} ->
                    process_past_blocks_(SubscribeBlock, TxnHash, TxnTypes, HeadHeight, Chain, StreamState);
                {error, not_found} ->
                    case blockchain:find_first_block_after(Height, Chain) of
                        {ok, _Height, ClosestBlock} ->
                            process_past_blocks_(ClosestBlock, <<>>, TxnTypes, HeadHeight, Chain, StreamState);
                        {error, _} = Error -> Error
                    end
            end
    end;
process_past_blocks(_Height, _TxnHash, _TxnTypes, _Chain, _StreamState) -> {error, invalid_req_params}.

-spec process_past_blocks_(StartBlock  :: blockchain_block:block(),
                           TxnHash     :: binary(),
                           TxnTypes    :: [atom()],
                           HeadHeight  :: pos_integer(),
                           Chain       :: blockchain:blockchain(),
                           StreamState :: grpcbox_stream:t()) -> {ok, grpcbox_stream:t()}.
process_past_blocks_(StartBlock, TxnHash, TxnTypes, HeadHeight, Chain, StreamState) ->
    StartHeight = blockchain_block:height(StartBlock),
    StartBlockTimestamp = blockchain_block:time(StartBlock),
    SortedStartTxns = filter_hash_sort_txns(StartBlock, TxnTypes),
    {UnhandledTxns, _} = lists:partition(fun({H, _T}) -> H > TxnHash end, SortedStartTxns),
    StreamState1 = send_txn_sequence(UnhandledTxns, StartHeight, StartBlockTimestamp, StreamState),
    BlockSeq = lists:seq(StartHeight + 1, HeadHeight),
    StreamState2 = lists:foldl(fun(HeightX, StateAcc) ->
                                   {ok, BlockX} = blockchain:get_block(HeightX, Chain),
                                   BlockXTimestamp = blockchain_block:time(BlockX),
                                   SortedTxnsX = filter_hash_sort_txns(BlockX, TxnTypes),
                                   _NewStateAcc = send_txn_sequence(SortedTxnsX, HeightX, BlockXTimestamp, StateAcc)
                               end, StreamState1, BlockSeq),
    {ok, StreamState2}.

-spec filter_hash_sort_txns(blockchain_block:block(), [atom()]) -> [{binary(), blockchain_txn:txn()}].
filter_hash_sort_txns(Block, TxnTypes) ->
    Txns = blockchain_block:transactions(Block),
    FilteredTxns = lists:filter(fun(Txn) -> subscribed_type(blockchain_txn:type(Txn), TxnTypes) end, Txns),
    HashKeyedTxns = lists:map(fun(Txn) -> {blockchain_txn:hash(Txn), Txn} end, FilteredTxns),
    lists:sort(fun({H1, _T1}, {H2, _T2}) -> H1 < H2 end, HashKeyedTxns).

-spec send_txn_sequence(SortedTxns :: [{binary(), blockchain_txn:txn()}],
                        Height :: pos_integer(),
                        Timestamp :: pos_integer(),
                        StreamState :: grpcbox_stream:t()) -> grpcbox_stream:t().
send_txn_sequence(SortedTxns, Height, Timestamp, StreamState) ->
    lists:foldl(fun({TxnHash, Txn}, StateAcc) ->
                    Msg = encode_follower_resp(TxnHash, Txn, Height, Timestamp),
                    grpcbox_stream:send(false, Msg, StateAcc)
                end, StreamState, SortedTxns).

-spec encode_follower_resp(TxnHash :: binary(),
                           Txn :: blockchain_txn:txn(),
                           TxnHeight :: pos_integer(),
                           Timestamp :: pos_integer()) -> follower_pb:follower_resp_v1_pb().
encode_follower_resp(TxnHash, Txn, TxnHeight, Timestamp) ->
    #follower_txn_stream_resp_v1_pb{
        height = TxnHeight,
        txn_hash = TxnHash,
        txn = blockchain_txn:wrap_txn(Txn),
        timestamp = Timestamp
    }.

subscribed_type(_Type, []) -> true;
subscribed_type(Type, FilterTypes) -> lists:member(Type, FilterTypes).

validate_txn_filters(TxnFilters0) ->
    case
        (catch lists:foldl(
            fun
                (BinType, AtomTypes) when is_binary(BinType) ->
                    [binary_to_existing_atom(BinType, utf8) | AtomTypes];
                (BinType, AtomTypes) when is_list(BinType) ->
                    [list_to_existing_atom(BinType) | AtomTypes]
            end,
            [],
            TxnFilters0
        ))
    of
        {'EXIT', _} ->
            {error, invalid_filter};
        TxnFilters when is_list(TxnFilters) ->
            case lists:all(fun is_blockchain_txn/1, TxnFilters) of
                true -> {ok, TxnFilters};
                false -> {error, invalid_filters}
            end
    end.

is_blockchain_txn(Module) ->
    ModInfo = Module:module_info(attributes),
    lists:any(fun({behavior, [blockchain_txn]}) -> true; (_) -> false end, ModInfo).

%% blockchain_region_v1 returns region as an atom with a 'region_' prefix, ie
%% 'region_us915' etc, we need it without the prefix and capitalised to
%% be compatible with the proto
normalize_region(V) ->
    list_to_atom(string:to_upper(string:slice(atom_to_list(V), 7))).

-spec region_params_for_region(atom(), blockchain_ledger_v1:ledger()) ->
    {ok, [blockchain_region_param_v1:region_param_v1()]} | {error, no_params_for_region}.
region_params_for_region(Region, Ledger) ->
    case blockchain_region_params_v1:for_region(Region, Ledger) of
        {error, Reason} ->
            lager:error(
                "Could not get params for region: ~p, reason: ~p",
                [Region, Reason]
            ),
            {error, no_params_for_region};
        {ok, Params} ->
            {ok, Params}
    end.
