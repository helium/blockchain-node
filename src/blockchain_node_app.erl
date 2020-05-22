-module(blockchain_node_app).

-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    bn_sup:start_link().

stop(_State) ->
    ok.
