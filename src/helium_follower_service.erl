%%%-------------------------------------------------------------------
%%
%% Handler module for the follower service's streaming API/RPC
%%
%%%-------------------------------------------------------------------
-module(helium_follower_service).

-behaviour(helium_follower_bhvr).

-include("grpc/autogen/server/follower_pb.hrl").
-include_lib("blockchain/include/blockchain.hrl").

-export([txn_stream/2]).

-export([init/2, handle_info/2]).

-type handler_state() :: #{
    chain => blockchain:blockchain(),
    sig_fun => function(),
    streaming_initialized => boolean(),
    txn_types => [atom()]
}.

-export_type([handler_state/0]).

%% -------------------------------------------------------------------
%% helium_follower_bhvr callback functions
%% -------------------------------------------------------------------
-spec txn_stream(follower_pb:follower_txn_stream_req_v1_pb(), grpcbox_stream:t()) ->
        {ok, grpcbox_stream:t()} | grpcbox_stream:grpc_error_response().
txn_stream(#follower_txn_stream_req_v1_pb{} = Msg, StreamState) ->
    #{chain := Chain,
      sig_fun := SigFun,
      streaming_initialized := StreamInitialized} = grpcbox_stream:stream_handler_state(StreamState),
    txn_stream(Chain, StreamInitialized, SigFun, Msg, StreamState);
txn_stream(_Msg, StreamState) ->
    lager:warning("unhandled grpc msg ~p", [_Msg]),
    {ok, StreamState}.

-spec init(atom(), grpcbox_stream:t()) -> grpcbox_stream:t().
init(_RPC, StreamState) ->
    lager:debug("handler init, stream state ~p", [StreamState]),
    Chain = blockchain_worker:blockchain(),
    {ok, _, SigFun, _} = blockchain_swarm:keys(),
    NewStreamState = grpcbox_stream:stream_handler_state(
                         StreamState,
                         #{chain => Chain,
                           sig_fun => SigFun,
                           streaming_initialized => false}
                     ),
    NewStreamState.

handle_info({blockchain_event, {add_block, BlockHash, _Sync, _Ledger}}, StreamState) ->
    #{chain := Chain, sig_fun := SigFun} = grpcbox_stream:stream_handler_state(StreamState),
    case blockchain:get_block(BlockHash, Chain) of
        {ok, Block} ->
            {HeadHeight, BlockTime} = head_info(Chain),
            Height = blockchain_block:height(Block),
            EpochStart = blockchain_block_v1:epoch_start(Block),
            SortedTxns = hash_and_sort_txns(Block),
            _NewStreamState = send_txn_sequence(SortedTxns, Height, HeadHeight, BlockTime, EpochStart, SigFun, StreamState);
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
-spec txn_stream(blockchain:blockchain() | undefined, boolean(), fun(), follower_pb:follower_txn_stream_req_v1_pb(), grpcbox_stream:t()) ->
        {ok, grpcbox_stream:t()} | grpcbox_stream:grpc_error_response().
txn_stream(undefined = _Chain, _StreamInitialized, _SigFun, _Msg, _StreamState) ->
    lager:debug("chain not ready, returning error response for msg ~p", [_Msg]),
    {grpc_error, {grpcbox_stream:code_to_status(14), <<"temporarily unavailable">>}};
txn_stream(_Chain, true = _StreamInitialized, _SigFun, _Msg, StreamState) ->
    {ok, StreamState};
txn_stream(
    Chain,
    false = _StreamInitialized,
    SigFun,
    #follower_txn_stream_req_v1_pb{height = Height, txn_hash = Hash, txn_types = TxnTypes} = _Msg,
    StreamState
) ->
    lager:debug("subscribing client to txn stream with msg ~p", [_Msg]),
    ok = blockchain_event:add_handler(self()),
    case process_past_blocks(Height, Hash, SigFun, Chain, StreamState) of
        {ok, StreamState1} ->
            HandlerState = grpcbox_stream:stream_handler_state(StreamState1),
            StreamState2 = grpcbox_stream:stream_handler_state(
                               StreamState1,
                               HandlerState#{streaming_initialized => true, txn_types => TxnTypes}
                           ),
            {ok, StreamState2};
        {error, _} ->
            {grpc_error, {grpcbox_stream:code_to_status(5), <<"requested block not found">>}}
    end.

-spec process_past_blocks(Height      :: pos_integer() | undefined,
                          TxnHash     :: binary(),
                          SigFun      :: fun(),
                          Chain       :: blockchain:blockchain(),
                          StreamState :: grpcbox_stream:t()) -> {ok, grpcbox_stream:t()} | {error, term()}.
process_past_blocks(undefined = _Height, _TxnHash, _SigFun, _Chain, StreamState) ->
    {ok, StreamState};
process_past_blocks(Height, TxnHash, SigFun, Chain, StreamState) when is_integer(Height) andalso Height > 0 ->
    {HeadHeight, BlockTime} = head_info(Chain),
    case Height > HeadHeight of
        %% requested a future block; nothing to do but wait
        true -> {ok, StreamState};
        false ->
            case blockchain:get_block(Height, Chain) of
                {ok, SubscribeBlock} ->
                    process_past_blocks_(SubscribeBlock, TxnHash, HeadHeight, BlockTime, SigFun, Chain, StreamState);
                {error, not_found} ->
                    case blockchain:find_first_block_after(Height, Chain) of
                        {ok, _Height, ClosestBlock} ->
                            process_past_blocks_(ClosestBlock, <<>>, HeadHeight, BlockTime, SigFun, Chain, StreamState);
                        {error, _} = Error -> Error
                    end
            end
    end.

-spec process_past_blocks_(StartBlock  :: blockchain_block:block(),
                           TxnHash      :: binary(),
                           HeadHeight  :: pos_integer(),
                           BlockTime   :: non_neg_integer(),
                           SigFun      :: fun(),
                           Chain       :: blockchain:blockchain(),
                           StreamState :: grpcbox_stream:t()) -> {ok, grpcbox_stream:t()}.
process_past_blocks_(StartBlock, TxnHash, HeadHeight, BlockTime, SigFun, Chain, StreamState) ->
    StartHeight = blockchain_block:height(StartBlock),
    BlockEpoch = blockchain_block_v1:epoch_start(StartBlock),
    SortedStartTxns = hash_and_sort_txns(StartBlock),
    {UnhandledTxns, _} = lists:partition(fun({H, _T}) -> H > TxnHash end, SortedStartTxns),
    StreamState1 = send_txn_sequence(UnhandledTxns, StartHeight, HeadHeight, BlockTime, BlockEpoch, SigFun, StreamState),
    BlockSeq = lists:seq(StartHeight + 1, HeadHeight),
    StreamState2 = lists:foldl(fun(HeightX, StateAcc) ->
                                   {ok, BlockX} = blockchain:get_block(HeightX, Chain),
                                   EpochX = blockchain_block_v1:epoch_start(BlockX),
                                   SortedTxnsX = hash_and_sort_txns(BlockX),
                                   _NewStateAcc = send_txn_sequence(SortedTxnsX, HeightX, HeadHeight, BlockTime, EpochX, SigFun, StateAcc)
                               end, StreamState1, BlockSeq),
    {ok, StreamState2}.

-spec hash_and_sort_txns(blockchain_block:block()) -> [{binary(), blockchain_txn:txn()}].
hash_and_sort_txns(Block) ->
    Txns = blockchain_block:transactions(Block),
    HashKeyedTxns = lists:map(fun(Txn) -> {blockchain_txn:hash(Txn), Txn} end, Txns),
    lists:sort(fun({H1, _T1}, {H2, _T2}) -> H1 < H2 end, HashKeyedTxns).

-spec send_txn_sequence(SortedTxns :: [{binary(), blockchain_txn:txn()}],
                        Height :: pos_integer(),
                        HeadHeight :: pos_integer(),
                        BlockTime :: non_neg_integer(),
                        EpochStart :: pos_integer(),
                        SigFun :: fun(),
                        StreamState :: grpcbox_stream:t()) -> grpcbox_stream:t().
send_txn_sequence(SortedTxns, Height, HeadHeight, BlockTime, EpochStart, SigFun, StreamState) ->
    lists:foldl(fun(Txn, StateAcc) ->
                    Msg = encode_follower_resp(Txn, Height, HeadHeight, BlockTime, EpochStart, SigFun),
                    grpcbox_stream:send(false, Msg, StateAcc)
                end, StreamState, SortedTxns).

-spec encode_follower_resp({TxnHash :: binary(), Txn :: blockchain_txn:txn()},
                           TxnHeight :: pos_integer(),
                           HeadHeight :: pos_integer(),
                           BlockTime :: non_neg_integer(),
                           EpochStart :: pos_integer(),
                           SigFun :: fun()) -> follower_pb:follower_resp_v1_pb().
encode_follower_resp({TxnHash, Txn}, TxnHeight, HeadHeight, BlockTime, EpochStart, SigFun) ->
    BlockAge = erlang:system_time(seconds) - BlockTime,
    WrappedEvent = #follower_txn_streamed_resp_v1_pb{
        height = TxnHeight,
        epoch = EpochStart,
        txn_hash = TxnHash,
        txn = blockchain_txn:wrap_txn(Txn)
    },
    Resp = #follower_resp_v1_pb{
        height = HeadHeight,
        signature = <<>>,
        block_time = BlockTime,
        block_age = BlockAge,
        event = {txn_event, WrappedEvent}
    },
    EncodedRespBin = follower_pb:encode_msg(Resp, follower_resp_v1_pb),
    Resp#follower_resp_v1_pb{signature = SigFun(EncodedRespBin)}.

-spec head_info(Chain :: blockchain:blockchain()) -> {HeadHeight :: pos_integer(), BlockTime :: pos_integer()}.
head_info(Chain) ->
    {ok, #block_info_v2{time = BlockTime, height = BlockHeight}} = blockchain:head_block_info(Chain),
    {BlockHeight, BlockTime}.
