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
    #{chain := Chain, streaming_initialized := StreamInitialized} =
        grpcbox_stream:stream_handler_state(StreamState),
    txn_stream(Chain, StreamInitialized, Msg, StreamState);
txn_stream(_Msg, StreamState) ->
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
            SortedTxns = filter_hash_sort_txns(Block, TxnTypes),
            _NewStreamState = send_txn_sequence(SortedTxns, Height, StreamState);
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
-spec txn_stream(blockchain:blockchain() | undefined, boolean(), follower_pb:follower_txn_stream_req_v1_pb(), grpcbox_stream:t()) ->
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
    TxnTypes = lists:foldl(fun(BinType, AtomTypes) ->
                               case (catch binary_to_existing_atom(BinType, utf8)) of
                                   {'EXIT', _} ->
                                       case (catch list_to_existing_atom(BinType)) of
                                           {'EXIT', _} -> AtomTypes;
                                           ListToAtom when is_atom(ListToAtom) -> [ListToAtom | AtomTypes]
                                       end;
                                   AtomType when is_atom(AtomType) -> [AtomType | AtomTypes]
                               end
                           end, [], TxnTypes0),
    case process_past_blocks(Height, Hash, TxnTypes, Chain, StreamState) of
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
                          TxnTypes    :: [atom()],
                          Chain       :: blockchain:blockchain(),
                          StreamState :: grpcbox_stream:t()) -> {ok, grpcbox_stream:t()} | {error, term()}.
process_past_blocks(undefined = _Height, _TxnHash, _TxnTypes, _Chain, StreamState) ->
    {ok, StreamState};
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
    end.

-spec process_past_blocks_(StartBlock  :: blockchain_block:block(),
                           TxnHash     :: binary(),
                           TxnTypes    :: [atom()],
                           HeadHeight  :: pos_integer(),
                           Chain       :: blockchain:blockchain(),
                           StreamState :: grpcbox_stream:t()) -> {ok, grpcbox_stream:t()}.
process_past_blocks_(StartBlock, TxnHash, TxnTypes, HeadHeight, Chain, StreamState) ->
    StartHeight = blockchain_block:height(StartBlock),
    SortedStartTxns = filter_hash_sort_txns(StartBlock, TxnTypes),
    {UnhandledTxns, _} = lists:partition(fun({H, _T}) -> H > TxnHash end, SortedStartTxns),
    StreamState1 = send_txn_sequence(UnhandledTxns, StartHeight, StreamState),
    BlockSeq = lists:seq(StartHeight + 1, HeadHeight),
    StreamState2 = lists:foldl(fun(HeightX, StateAcc) ->
                                   {ok, BlockX} = blockchain:get_block(HeightX, Chain),
                                   SortedTxnsX = filter_hash_sort_txns(BlockX, TxnTypes),
                                   _NewStateAcc = send_txn_sequence(SortedTxnsX, HeightX, StateAcc)
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
                        StreamState :: grpcbox_stream:t()) -> grpcbox_stream:t().
send_txn_sequence(SortedTxns, Height, StreamState) ->
    lists:foldl(fun({TxnHash, Txn}, StateAcc) ->
                    Msg = encode_follower_resp(TxnHash, Txn, Height),
                    grpcbox_stream:send(false, Msg, StateAcc)
                end, StreamState, SortedTxns).

-spec encode_follower_resp(TxnHash :: binary(),
                           Txn :: blockchain_txn:txn(),
                           TxnHeight :: pos_integer()) -> follower_pb:follower_resp_v1_pb().
encode_follower_resp(TxnHash, Txn, TxnHeight) ->
    #follower_txn_stream_resp_v1_pb{
        height = TxnHeight,
        txn_hash = TxnHash,
        txn = blockchain_txn:wrap_txn(Txn)
    }.

subscribed_type(_Type, []) -> true;
subscribed_type(Type, FilterTypes) -> lists:member(Type, FilterTypes).
