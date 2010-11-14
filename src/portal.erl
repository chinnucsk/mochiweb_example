%% @author panfu <panfu.gz@gmail.com>
%% @copyright panfu.

%% @doc TEMPLATE.

-module(portal).
-author('panfu <panfu.gz@gmail.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start() -> ok
%% @doc Start the portal server.
start() ->
    portal_deps:ensure(),
    ensure_started(crypto),
    application:start(portal).

%% @spec stop() -> ok
%% @doc Stop the portal server.
stop() ->
    Res = application:stop(portal),
    application:stop(crypto),
    Res.
