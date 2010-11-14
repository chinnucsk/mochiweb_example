%% @author panfu <panfu.gz@gmail.com>
%% @copyright panfu.

%% @doc Callbacks for the portal application.

-module(portal_app).
-author('panfu <panfu.gz@gmail.com>').

-behaviour(application).
-export([start/2, stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for portal.
start(_Type, _StartArgs) ->
    portal_deps:ensure(),
    portal_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for portal.
stop(_State) ->
    ok.


%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
