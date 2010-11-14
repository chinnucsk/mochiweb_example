-module(erlmongo_runner).

-export([start/0]).

start() ->
    {ok, spawn(fun startDB/0)}.

startDB() ->
    application:start(erlmongo),
    mongodb:singleServer(def),
    mongodb:connect(def).



