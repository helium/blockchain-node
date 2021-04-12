-module(bn_jsonrpc_handler).

-callback handle_rpc(Method :: binary(), Params :: term()) -> jsone:json().

-export([handle/2, handle_event/3]).
-export([
    jsonrpc_b58_to_bin/2,
    jsonrpc_b64_to_bin/2,
    jsonrpc_get_param/2,
    jsonrpc_get_param/3,
    jsonrpc_error/1
]).

-include("bn_jsonrpc.hrl").

-include_lib("elli/include/elli.hrl").

-behaviour(elli_handler).

handle(Req, _Args) ->
    %% Delegate to our handler function
    handle(Req#req.method, elli_request:path(Req), Req).

handle('POST', _, Req) ->
    Json = elli_request:body(Req),
    {reply, Reply} =
        jsonrpc2:handle(Json, fun handle_rpc/2, fun decode_helper/1, fun encode_helper/1),
    {ok, [], Reply};
handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.

handle_rpc(<<"block_", _/binary>> = Method, Params) ->
    bn_blocks:handle_rpc(Method, Params);
handle_rpc(<<"transaction_", _/binary>> = Method, Params) ->
    bn_txns:handle_rpc(Method, Params);
handle_rpc(<<"implicit_burn_", _/binary>> = Method, Params) ->
    bn_implicit_burn:handle_rpc(Method, Params);
handle_rpc(<<"pending_transaction_", _/binary>> = Method, Params) ->
    bn_pending_txns:handle_rpc(Method, Params);
handle_rpc(<<"account_", _/binary>> = Method, Params) ->
    bn_accounts:handle_rpc(Method, Params);
handle_rpc(<<"wallet_", _/binary>> = Method, Params) ->
    bn_wallets:handle_rpc(Method, Params);
handle_rpc(<<"oracle_price_", _/binary>> = Method, Params) ->
    bn_oracle_price:handle_rpc(Method, Params);
handle_rpc(_, _) ->
    ?jsonrpc_error(method_not_found).

%% @doc Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return `ok'.
handle_event(request_throw, [Req, Exception, Stack], _Config) ->
    lager:error("exception: ~p~nstack: ~p~nrequest: ~p~n", [
        Exception,
        Stack,
        elli_request:to_proplist(Req)
    ]),
    ok;
handle_event(request_exit, [Req, Exit, Stack], _Config) ->
    lager:error("exit: ~p~nstack: ~p~nrequest: ~p~n", [
        Exit,
        Stack,
        elli_request:to_proplist(Req)
    ]),
    ok;
handle_event(request_error, [Req, Error, Stack], _Config) ->
    lager:error("error: ~p~nstack: ~p~nrequest: ~p~n", [
        Error,
        Stack,
        elli_request:to_proplist(Req)
    ]),
    ok;
handle_event(_, _, _) ->
    ok.

%%
%% Param conversion
%%
jsonrpc_get_param(Key, PropList) ->
    case proplists:get_value(Key, PropList, false) of
        false -> ?jsonrpc_error(invalid_params);
        V -> V
    end.

jsonrpc_get_param(Key, PropList, Default) ->
    proplists:get_value(Key, PropList, Default).

jsonrpc_b58_to_bin(Key, PropList) ->
    B58 = jsonrpc_get_param(Key, PropList),
    try ?B58_TO_BIN(B58)
    catch
        _:_ -> ?jsonrpc_error(invalid_params)
    end.

jsonrpc_b64_to_bin(Key, PropList) ->
    B64 = jsonrpc_get_param(Key, PropList),
    try ?B64_TO_BIN(B64)
    catch
        _:_ -> ?jsonrpc_error(invalid_params)
    end.

%%
%% Errors
%%
-define(throw_error(C, L), throw({jsonrpc2, C, iolist_to_binary((L))})).
-define(throw_error(C, F, A),
    throw({jsonrpc2, C, iolist_to_binary(io_lib:format((F), (A)))})
).

-define(ERR_NOT_FOUND, -100).
-define(ERR_INVALID_PASSWORD, -110).
-define(ERR_ERROR, -150).

-spec jsonrpc_error(term()) -> no_return().
jsonrpc_error(method_not_found = E) ->
    throw(E);
jsonrpc_error(invalid_params = E) ->
    throw(E);
jsonrpc_error({invalid_params, _} = E) ->
    throw(E);
jsonrpc_error({not_found, F, A}) ->
    ?throw_error(?ERR_NOT_FOUND, F, A);
jsonrpc_error({not_found, M}) ->
    ?throw_error(?ERR_NOT_FOUND, M);
jsonrpc_error(invalid_password) ->
    ?throw_error(?ERR_INVALID_PASSWORD, "Invalid password");
jsonrpc_error({error, F, A}) ->
    ?throw_error(?ERR_ERROR, F, A);
jsonrpc_error({error, E}) ->
    jsonrpc_error({error, "~p", E}).

%%
%% Internal
%%

decode_helper(Bin) ->
    jsone:decode(Bin, [{object_format, tuple}]).

encode_helper(Json) ->
    jsone:encode(Json, [undefined_as_null]).
