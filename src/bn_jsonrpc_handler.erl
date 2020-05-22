-module(bn_jsonrpc_handler).

-callback handle_rpc(Method::binary(), Params::list()) -> jsone:json().

-export([handle/2, handle_event/3]).

-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

handle(Req, _Args) ->
    %% Delegate to our handler function
    handle(Req#req.method, elli_request:path(Req), Req).


handle('POST',_, Req) ->
    Json = elli_request:body(Req),
    {reply, Reply} = jsonrpc2:handle(Json, fun handle_rpc/2, fun decode_helper/1, fun encode_helper/1),
    {ok, [], Reply};

handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.


handle_rpc(<<"block_", _/binary>>=Method, Params) ->
    bn_blocks:handle_rpc(Method, Params);

handle_rpc(<<"transaction_", _/binary>>=Method, Params) ->
    bn_txns:handle_rpc(Method, Params);

handle_rpc(<<"account_", _/binary>>=Method, Params) ->
    bn_accounts:handle_rpc(Method, Params);

handle_rpc(<<"wallet_", _/binary>>=Method, Params) ->
    bn_wallets:handle_rpc(Method, Params);

handle_rpc(_, _) ->
    throw(method_not_found).


%% @doc Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return `ok'.
handle_event(request_throw, [Req, Exception, Stack], _Config) ->
    lager:error("exception: ~p~nstack: ~p~nrequest: ~p~n",
                           [Exception, Stack, elli_request:to_proplist(Req)]),
    ok;
handle_event(request_exit, [Req, Exit, Stack], _Config) ->
    lager:error("exit: ~p~nstack: ~p~nrequest: ~p~n",
                           [Exit, Stack, elli_request:to_proplist(Req)]),
    ok;
handle_event(request_error, [Req, Error, Stack], _Config) ->
    lager:error("error: ~p~nstack: ~p~nrequest: ~p~n",
                           [Error, Stack, elli_request:to_proplist(Req)]),
    ok;

handle_event(_, _, _) ->
    ok.


%%
%% Internal
%%

decode_helper(Bin) ->
    jsone:decode(Bin, [{object_format, tuple}]).

encode_helper(Json) ->
    jsone:encode(Json, [undefined_as_null]).
