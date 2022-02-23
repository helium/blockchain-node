-module(bn_gateways).

-include("bn_jsonrpc.hrl").
%% blockchain_follower

-behavior(bn_jsonrpc_handler).

-export([
    requires_sync/0,
    requires_ledger/0,
    init/1,
    follower_height/1,
    load_chain/2,
    load_block/5,
    terminate/2
]).

%% jsonrpc_handler
-export([handle_rpc/2]).
% api
-export([get_historic_gateway_info/2]).
% hooks
-export([incremental_commit_hook/2, end_commit_hook/3]).

-define(DB_FILE, "historic_gateways.db").
-define(SERVER, ?MODULE).

-record(state, {
    dir :: file:filename_all(),
    db :: rocksdb:db_handle(),
    default :: rocksdb:cf_handle(),
    historic_gateways :: rocksdb:cf_handle()
}).

%%
%% Blockchain follower
%%

requires_ledger() -> false.

requires_sync() -> false.

init(Args) ->
    Dir = filename:join(proplists:get_value(base_dir, Args, "data"), ?DB_FILE),
    case load_db(Dir) of
        {ok, State} ->
            ets:new(?MODULE, [public, named_table]),
            persistent_term:put(?MODULE, State),
            {ok, State};
        Error ->
            Error
    end.

follower_height(#state{db = DB, default = DefaultCF}) ->
    case bn_db:get_follower_height(DB, DefaultCF) of
        {ok, Height} -> Height;
        {error, _} = Error -> ?jsonrpc_error(Error)
    end.

load_chain(_Chain, State = #state{}) ->
    {ok, State}.

load_block(_Hash, Block, _Sync, _Ledger, State = #state{
    db=DB, 
    default=DefaultCF, 
    historic_gateways=HistoricGatewaysCF
}) ->
    Height = blockchain_block:height(Block),
    case blockchain:ledger_at(Height, blockchain_worker:blockchain()) of
        {error, _} -> 
            lager:info("No ledger available at height ~p.", [Height]),
            ok;
        {ok, Ledger} ->
            case rocksdb:get(DB, DefaultCF, <<"loaded_initial_gateways">>, []) of
                not_found ->
                    {ok, Batch} = rocksdb:batch(),
                    lager:info("Loading initial gateways at height ~p", [Height]),
                    GatewaySnapshotList = blockchain_ledger_v1:snapshot_raw_gateways(Ledger),
                    lists:foreach(
                        fun({AddressBin, GWInfoBin}) ->
                            HeightEntryKeyBin = <<AddressBin/binary, Height:64/integer-unsigned-big>>,
                            rocksdb:put(DB, HistoricGatewaysCF, HeightEntryKeyBin, GWInfoBin, [])
                        end,
                        GatewaySnapshotList
                    ),
                    lager:info("Finished saving initial gateways"),
                    rocksdb:batch_put(Batch, <<"loaded_initial_gateways">>, <<"true">>),
                    rocksdb:write_batch(DB, Batch, []);
                _ ->
                    ok
            end
    end,
    bn_db:put_follower_height(DB, DefaultCF, Height),
    {ok, State}.

terminate(_Reason, #state{db = DB}) ->
    rocksdb:close(DB).

%%
%% Hooks
%%

incremental_commit_hook(_Changes, _Height) -> 
    ok.

end_commit_hook(_CF, Changes, Height) ->
    {ok, #state{db=DB}} = get_state(),
    {ok, Batch} = rocksdb:batch(),
    lists:foldl(
        fun
            ({put, Key}, Acc) -> 
                batch_update_entry(Key, Batch, Height),
                Acc;
            (_, Acc) -> Acc
        end,
        [],
        Changes
    ),
    rocksdb:write_batch(DB, Batch, []).

%%
%% jsonrpc_handler
%%

handle_rpc(<<"gateway_info_get">>, {Param}) ->
    Chain = blockchain_worker:blockchain(),
    {Height, Ledger} =
        case ?jsonrpc_get_param(<<"height">>, Param, false) of
            false ->
                {bn_txns:follower_height(), blockchain:ledger(Chain)};
            V ->
                case blockchain:ledger_at(V, Chain) of
                    {ok, OldLedger} ->
                        {V, OldLedger};
                    {error, height_too_old} ->
                        case application:get_env(blockchain, store_historic_gateways, false) of
                            true ->
                                {V, {error, height_too_old}};
                            false ->
                                {V, ?jsonrpc_error(
                                    {error, "height ~p is too old to get historic gateway data", [V]}
                                )}
                        end;
                    {error, _} = Error ->
                        {V, ?jsonrpc_error(Error)}
                end
        end,
    Address = ?jsonrpc_b58_to_bin(<<"address">>, Param),
    case Ledger of
        {error, height_too_old} ->
            case get_historic_gateway_info(Address, Height) of
                {ok, GWInfoBin} ->
                    GWInfo = blockchain_ledger_entry_v1:deserialize(GWInfoBin),
                    #{
                        owner_address => blockchain_ledger_gateway_v2:owner_address(GWInfo),
                        location => ?MAYBE_H3(blockchain_ledger_gateway_v2:location(GWInfo)),
                        alpha => blockchain_ledger_gateway_v2:alpha(GWInfo),
                        beta => blockchain_ledger_gateway_v2:beta(GWInfo),
                        delta => blockchain_ledger_gateway_v2:delta(GWInfo),
                        last_poc_challenge => blockchain_ledger_gateway_v2:last_poc_challenge(GWInfo),
                        last_poc_onion_key_hash => blockchain_ledger_gateway_v2:last_poc_onion_key_hash(GWInfo),
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
                    ?jsonrpc_error({error, "unable to retrieve account details for ~p at height ~p due to error: ~p", [?BIN_TO_B58(Address), Height, E]})
            end;
        _ ->
            case blockchain_ledger_v1:find_gateway_info(Address, Ledger) of
                {ok, GWInfo} ->
                    #{
                        owner_address => ?BIN_TO_B58(blockchain_ledger_gateway_v2:owner_address(GWInfo)),
                        location => ?MAYBE_H3(blockchain_ledger_gateway_v2:location(GWInfo)),
                        alpha => blockchain_ledger_gateway_v2:alpha(GWInfo),
                        beta => blockchain_ledger_gateway_v2:beta(GWInfo),
                        delta => blockchain_ledger_gateway_v2:delta(GWInfo),
                        last_poc_challenge => blockchain_ledger_gateway_v2:last_poc_challenge(GWInfo),
                        last_poc_onion_key_hash => ?BIN_TO_B64(blockchain_ledger_gateway_v2:last_poc_onion_key_hash(GWInfo)),
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
                    ?jsonrpc_error({error, "unable to retrieve account details for ~p at height ~p due to error: ~p", [?BIN_TO_B58(Address), Height, E]});
                _ ->
                    ?jsonrpc_error({error, "unable to retrieve account details for ~p at height ~p due to unknown error.", [?BIN_TO_B58(Address), Height]})
            end
    end;
handle_rpc(_, _) ->
    ?jsonrpc_error(method_not_found).


%%
%% Internal
%%

get_state() ->
    bn_db:get_state(?MODULE).

-spec load_db(file:filename_all()) -> {ok, #state{}} | {error, any()}.
load_db(Dir) ->
    case bn_db:open_db(Dir, ["default", "historic_gateways"], [{prefix_transform, {fixed_prefix_transform, 33}}]) of
        {error, _Reason} = Error ->
            Error;
        {ok, DB, [DefaultCF, HistoricGatewaysCF]} ->
            State = #state{
                dir = Dir,
                db = DB,
                default = DefaultCF,
                historic_gateways = HistoricGatewaysCF
            },
            compact_db(State),
            {ok, State}
    end.

batch_update_entry(Key, Batch, Height) ->
    {ok, #state{historic_gateways=HistoricGatewaysCF}} = get_state(),
    HeightEntryKeyBin = <<Key/binary, Height:64/integer-unsigned-big>>,
    {ok, Ledger} = blockchain:ledger_at(Height, blockchain_worker:blockchain()),
    {ok, GWInfo} = blockchain_ledger_v1:find_gateway_info(Key, Ledger),
    GWInfoBin = blockchain_ledger_gateway_v2:serialize(GWInfo),
    rocksdb:batch_put(Batch, HistoricGatewaysCF, HeightEntryKeyBin, GWInfoBin).

-spec get_historic_gateway_info(Key :: binary(), Height :: pos_integer()) ->
    {ok, {binary(), binary(), binary()}} | {error, term()}.
get_historic_gateway_info(Key, Height0) ->
    {ok, #state{
        db=DB,
        historic_gateways=HistoricGatewaysCF
    }} = get_state(),
    % Set ledger to 2nd block if genesis block is selected
    Height = case Height0 of
        1 ->
            2;
        _ ->
            Height0
    end,
    {ok, GatewayIterator} = rocksdb:iterator(DB, HistoricGatewaysCF, [{iterate_lower_bound, <<Key/binary, 0:64/integer-unsigned-big>>}, {total_order_seek, true}]),
    case rocksdb:iterator_move(GatewayIterator, {seek_for_prev, <<Key/binary, Height:64/integer-unsigned-big>>}) of
        {ok, _, GWInfoBin} ->
            {ok, GWInfoBin};
        {ok, _} ->
            {error, invalid_entry};
        {error, Error} ->
            {error, Error}
    end.

compact_db(#state{
    db = DB,
    default = Default,
    historic_gateways=HistoricGatewaysCF
}) ->
    rocksdb:compact_range(DB, Default, undefined, undefined, []),
    rocksdb:compact_range(DB, HistoricGatewaysCF, undefined, undefined, []),
    ok.
