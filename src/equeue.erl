-module(equeue).

-export([start/0, pop/0, pop/1, add/1, loop/1]).

start() ->
    {ok, spawn(fun startRq/0)}.

startRq() ->
    redis:connect([]).

add(MdbId) ->
    redis:q([sadd, "equeue", MdbId]).

pop() ->
    pop(0).

pop(0) ->
    redis:q([spop, "equeue"]);
pop(N) ->
    io:format("pop: ~p~n", [redis:q([spop, "equeue"])]),
    pop(N - 1).

loop(N) ->
    pop(N),
    timer:sleep(1000),
    loop(N).

