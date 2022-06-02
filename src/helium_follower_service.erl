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
    lager:warning("unhandled msg ~p", [_Msg]),
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
            Height = blockchain_block:height(Block),
            EpochStart = blockchain_block_v1:epoch_start(Block),
            SortedTxns = hash_and_sort_txns(Block),
            _NewStreamState = send_txn_sequence(SortedTxns, Height, EpochStart, SigFun, Chain, StreamState);
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
    case process_previous_blocks(Height, Hash, SigFun, Chain, StreamState) of
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

-spec process_previous_blocks(Height      :: pos_integer() | undefined,
                              TxnHash     :: binary(),
                              SigFun      :: fun(),
                              Chain       :: blockchain:blockchain(),
                              StreamState :: grpcbox_stream:t()) -> {ok, grpcbox_stream:t()} | {error, term()}.
process_previous_blocks(undefined = _Height, _Hash, _SigFun, _Chain, StreamState) -> {ok, StreamState};
process_previous_blocks(Height, Hash, SigFun, Chain, StreamState) when is_integer(Height) andalso Height > 0 ->
    case blockchain:head_block(Chain) of
        {ok, HeadBlock} ->
            HeadHeight = blockchain_block:height(HeadBlock),
            case blockchain:get_block(Height, Chain) of
                {ok, PriorBlock} ->
                    PriorBlockEpoch = blockchain_block_v1:epoch_start(PriorBlock),
                    PriorBlockTxns = blockchain_block:transactions(PriorBlock),
                    SortedPriorBlockTxns = hash_and_sort_txns(PriorBlockTxns),
                    {UnhandledPriorTxns, _} = lists:partition(fun({H1, _T1}) -> H1 > Hash end, SortedPriorBlockTxns),
                    StreamState1 = send_txn_sequence(UnhandledPriorTxns, Height, PriorBlockEpoch, SigFun, Chain, StreamState),
                    BlockSeq = lists:seq(Height + 1, HeadHeight),
                    StreamState2 = lists:foldl(fun(HeightX, StateAcc) ->
                                                   {ok, BlockX} = blockchain:get_block(HeightX, Chain),
                                                   EpochX = blockchain_block_v1:epoch_start(BlockX),
                                                   TxnsX = blockchain_block:transactions(BlockX),
                                                   SortedTxnsX = hash_and_sort_txns(TxnsX),
                                                   _NewStateAcc = send_txn_sequence(SortedTxnsX, HeightX, EpochX, SigFun, Chain, StateAcc)
                                               end, StreamState1, BlockSeq),
                    {ok, StreamState2};
                {error, _} = Error -> Error
            end;
        {error, _} = Error -> Error
    end.

-spec hash_and_sort_txns(blockchain_block:block()) -> [{binary(), blockchain_txn:txn()}].
hash_and_sort_txns(Block) ->
    Txns = blockchain_block:transactions(Block),
    HashKeyedTxns = lists:map(fun(Txn) -> {blockchain_txn:hash(Txn), Txn} end, Txns),
    lists:sort(fun({H1, _T1}, {H2, _T2}) -> H1 < H2 end, HashKeyedTxns).

-spec send_txn_sequence(SortedTxns :: [{binary(), blockchain_txn:txn()}],
                        Height :: pos_integer(),
                        EpochStart :: pos_integer(),
                        SigFun :: fun(),
                        Chain :: blockchain:blockchain(),
                        StreamState :: grpcbox_stream:t()) -> grpcbox_stream:t().
send_txn_sequence(SortedTxns, Height, EpochStart, SigFun, Chain, StreamState) ->
    lists:foldl(fun(Txn, StateAcc) ->
                    Msg = encode_follower_resp(Txn, Height, EpochStart, SigFun, Chain),
                    grpcbox_stream:send(false, Msg, StateAcc)
                end, StreamState, SortedTxns).

-spec encode_follower_resp({TxnHash :: binary(), Txn :: blockchain_txn:txn()},
                           TxnHeight :: pos_integer(),
                           EpochStart :: pos_integer(),
                           SigFun :: fun(),
                           Chain :: blockchain:blockchain()) -> follower_pb:follower_resp_v1_pb().
encode_follower_resp({TxnHash, Txn}, TxnHeight, EpochStart, SigFun, Chain) ->
    {ok, #block_info_v2{time = BlockTime, height = BlockHeight}} = blockchain:head_block_info(Chain),
    BlockAge = erlang:system_time(seconds) - BlockTime,
    WrappedTxn = #follower_txn_streamed_resp_v1_pb{
        height = TxnHeight,
        epoch = EpochStart,
        txn_hash = TxnHash,
        txn = Txn
    },
    Resp = #follower_resp_v1_pb{
        height = BlockHeight,
        signature = <<>>,
        block_time = BlockTime,
        block_age = BlockAge,
        event = {txn_event, WrappedTxn}
    },
    EncodedRespBin = follower_pb:encode_msg(Resp, follower_resp_v1_pb),
    Resp#follower_resp_v1_pb{signature = SigFun(EncodedRespBin)}.
