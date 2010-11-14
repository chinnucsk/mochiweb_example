%% @author panfu <panfu.gz@gmail.com>
%% @copyright panfu.

%% @doc Supervisor for the portal application.

-module(portal_sup).
-author('panfu <panfu.gz@gmail.com>').

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    Ip = case os:getenv("MOCHIWEB_IP") of false -> "0.0.0.0"; Any -> Any end,
    WebConfig = [
                 {ip, Ip},
                 {port, 8000},
                 {docroot, portal_deps:local_path(["priv", "www"])}],
    Web = {portal_web,
           {portal_web, start, [WebConfig]},
           permanent, 5000, worker, dynamic},
    Mongo = {erlmongo_runner,
            {erlmongo_runner, start, []},
            permanent, 5000, worker, dynamic},
    Equeue= {equeue,
            {equeue, start, []},
            permanent, 5000, worker, dynamic},

    Processes = [Mongo, Equeue, Web],
    {ok, {{one_for_one, 10, 10}, Processes}}.


%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
