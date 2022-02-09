-module(bn_balances).

-include("bn_jsonrpc.hrl").
%% blockchain_follower
-export([
    requires_sync/0,
    requires_ledger/0,
    init/1,
    follower_height/1,
    load_chain/2,
    load_block/5,
    terminate/2
]).

% api
-export([get_historic_entry/2]).
% hooks
-export([incremental_commit_hook/2, end_commit_hook/3]).

-define(DB_FILE, "balances.db").
-define(SERVER, ?MODULE).

-record(state, {
    dir :: file:filename_all(),
    db :: rocksdb:db_handle(),
    default :: rocksdb:cf_handle(),
    entries :: rocksdb:cf_handle()
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
    entries=EntriesCF
}) ->
    Height = blockchain_block:height(Block),
    case blockchain:ledger_at(Height, blockchain_worker:blockchain()) of
        {error, _} ->            
            ok;
        {ok, Ledger} ->
            case rocksdb:get(DB, DefaultCF, <<"loaded_initial_balances">>, []) of
                not_found ->
                    {ok, Batch} = rocksdb:batch(),
                    lager:info("Loading initial balances at height ~p", [Height]),
                    EntrySnapshotList = blockchain_ledger_v1:snapshot_raw_accounts(Ledger),
                    DCSnapshotList = blockchain_ledger_v1:snapshot_raw_dc_accounts(Ledger),
                    SecuritySnapshotList = blockchain_ledger_v1:snapshot_raw_security_accounts(Ledger),
                    ZeroEntry = blockchain_ledger_entry_v1:new(0, 0),
                    ZeroEntryBin = blockchain_ledger_entry_v1:serialize(ZeroEntry),
                    DCZeroEntry = blockchain_ledger_data_credits_entry_v1:new(0, 0),
                    DCZeroEntryBin = blockchain_ledger_data_credits_entry_v1:serialize(DCZeroEntry),
                    SecurityZeroEntry = blockchain_ledger_security_entry_v1:new(0, 0),
                    SecurityZeroEntryBin = blockchain_ledger_security_entry_v1:serialize(SecurityZeroEntry),
                    lists:foreach(
                        fun({Entries, Type}) ->
                            lists:foldl(
                                fun({AddressBin, EntryBin}, Acc) ->
                                    HeightEntryKeyBin = <<AddressBin/binary, Height:64/integer-unsigned-big>>,
                                    HeightEntryValueBin = case Type of
                                        "entries" ->
                                            erlang:term_to_binary({EntryBin, DCZeroEntryBin, SecurityZeroEntryBin});
                                        "dcs" ->
                                            case rocksdb:get(DB, EntriesCF, HeightEntryKeyBin, []) of
                                                not_found ->
                                                    erlang:term_to_binary({ZeroEntryBin, EntryBin, SecurityZeroEntryBin});
                                                {ok, B} ->
                                                    {Entry, _, _} = erlang:binary_to_term(B),
                                                    erlang:term_to_binary({Entry, EntryBin, SecurityZeroEntryBin})
                                            end;
                                        "securities" ->
                                            case rocksdb:get(DB, EntriesCF, HeightEntryKeyBin, []) of
                                                not_found ->
                                                    erlang:term_to_binary({ZeroEntryBin, DCZeroEntryBin, EntryBin});
                                                {ok, B} ->
                                                    {Entry, DCEntry, _} = erlang:binary_to_term(B),
                                                    erlang:term_to_binary({Entry, DCEntry, EntryBin})
                                            end
                                    end,
                                    rocksdb:put(DB, EntriesCF, HeightEntryKeyBin, HeightEntryValueBin, []),
                                    Acc
                                end,
                                [],
                                Entries
                            )
                        end,
                        [
                            {EntrySnapshotList, "entries"},
                            {DCSnapshotList, "dcs"},
                            {SecuritySnapshotList, "securities"}
                        ]
                    ),
                    lager:info("Finished saving initial balances"),
                    rocksdb:batch_put(Batch, <<"loaded_initial_balances">>, <<"true">>),
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
%% Internal
%%

get_state() ->
    bn_db:get_state(?MODULE).

-spec load_db(file:filename_all()) -> {ok, #state{}} | {error, any()}.
load_db(Dir) ->
    case bn_db:open_db(Dir, ["default", "entries"], [{prefix_transform, {fixed_prefix_transform, 33}}]) of
        {error, _Reason} = Error ->
            Error;
        {ok, DB, [DefaultCF, EntriesCF]} ->
            State = #state{
                dir = Dir,
                db = DB,
                default = DefaultCF,
                entries = EntriesCF
            },
            compact_db(State),
            {ok, State}
    end.

batch_update_entry(Key, Batch, Height) ->
    {ok, #state{entries=EntriesCF}} = get_state(),
    HeightEntryKeyBin = <<Key/binary, Height:64/integer-unsigned-big>>,
    {ok, Ledger} = blockchain:ledger_at(Height, blockchain_worker:blockchain()),
    EntryBin = case blockchain_ledger_v1:find_entry(Key, Ledger) of
        {ok, Entry} ->
            blockchain_ledger_entry_v1:serialize(Entry);
        {error,address_entry_not_found} ->
            ZeroEntry = blockchain_ledger_entry_v1:new(0, 0),
            blockchain_ledger_entry_v1:serialize(ZeroEntry)
    end,
    DCBin = case blockchain_ledger_v1:find_dc_entry(Key, Ledger) of
        {ok, DCEntry} ->
            blockchain_ledger_data_credits_entry_v1:serialize(DCEntry);
        {error,dc_entry_not_found} ->
            DCZeroEntry = blockchain_ledger_data_credits_entry_v1:new(0, 0),
            blockchain_ledger_data_credits_entry_v1:serialize(DCZeroEntry)
    end,
    SecurityBin = case blockchain_ledger_v1:find_security_entry(Key, Ledger) of
        {ok, SecurityEntry} ->
            blockchain_ledger_security_entry_v1:serialize(SecurityEntry);
        {error,not_found} ->
            SecurityZeroEntry = blockchain_ledger_security_entry_v1:new(0, 0),
            blockchain_ledger_security_entry_v1:serialize(SecurityZeroEntry)
    end,
    rocksdb:batch_put(Batch, EntriesCF, HeightEntryKeyBin, erlang:term_to_binary({EntryBin, DCBin, SecurityBin})).

-spec get_historic_entry(Key :: binary(), Height :: pos_integer()) ->
    {ok, {binary(), binary(), binary()}} | {error, term()}.
get_historic_entry(Key, Height0) ->
    {ok, #state{
        db=DB,
        entries=EntriesCF
    }} = get_state(),
    % Set ledger to 2nd block if genesis block is selected
    Height = case Height0 of
        1 ->
            2;
        _ ->
            Height0
    end,
    {ok, BalanceIterator} = rocksdb:iterator(DB, EntriesCF, [{iterate_lower_bound, <<Key/binary, 0:64/integer-unsigned-big>>}, {total_order_seek, true}]),
    case rocksdb:iterator_move(BalanceIterator, {seek_for_prev, <<Key/binary, Height:64/integer-unsigned-big>>}) of
        {ok, _, EntryBin} ->
            {ok, erlang:binary_to_term(EntryBin)};
        {ok, _} ->
            {error, invalid_entry};
        {error, Error} ->
            {error, Error}
    end.

compact_db(#state{
    db = DB,
    default = Default,
    entries=EntriesCF
}) ->
    rocksdb:compact_range(DB, Default, undefined, undefined, []),
    rocksdb:compact_range(DB, EntriesCF, undefined, undefined, []),
    ok.
