-module(bn_db).

-export([open_db/2, clean_db/1]).

-spec open_db(Dir::file:filename_all(), CFNames::[string()])
             -> {ok, rocksdb:db_handle(), [rocksdb:cf_handle()]} | {error, any()}.
open_db(Dir, CFNames) ->
    ok = filelib:ensure_dir(Dir),
    GlobalOpts = application:get_env(rocksdb, global_opts, []),
    DBOptions = [{create_if_missing, true}, {atomic_flush, true}] ++ GlobalOpts,
    ExistingCFs =
        case rocksdb:list_column_families(Dir, DBOptions) of
            {ok, CFs0} ->
                CFs0;
            {error, _} ->
                ["default"]
        end,

    CFOpts = GlobalOpts,
    case rocksdb:open_with_cf(Dir, DBOptions,  [{CF, CFOpts} || CF <- ExistingCFs]) of
        {error, _Reason}=Error ->
            Error;
        {ok, DB, OpenedCFs} ->
            L1 = lists:zip(ExistingCFs, OpenedCFs),
            L2 = lists:map(
                fun(CF) ->
                    {ok, CF1} = rocksdb:create_column_family(DB, CF, CFOpts),
                    {CF, CF1}
                end,
                CFNames -- ExistingCFs
            ),
            L3 = L1 ++ L2,
            {ok, DB, [proplists:get_value(X, L3) || X <- CFNames]}
    end.

clean_db(Dir) when is_list(Dir) ->
    ok = rocksdb:destroy(Dir, []).
