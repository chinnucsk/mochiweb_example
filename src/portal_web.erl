%% @author panfu <panfu.gz@gmail.com>
%% @copyright panfu.

%% @doc Web server for portal.

-module(portal_web).
-author('panfu <panfu.gz@gmail.com>').

-export([start/1, stop/0, loop/2, get_options/2]).

-record(sources, {data, token, user_id, input_time, content}).

%% External API

start(Options) ->
    %% startMDB(),
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

startMDB() ->
    equeue:start().

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    Path = Req:get(path),
    Action = clean_path(Path),
    %% io:format("Req: ~p | ~p~n", [Path, Action]),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case Action of
                "/save" ->
                    Params = Req:parse_qs(),
                    %% io:format("Req: ~p~n", [Params]),
                    MongoServer = mongoapi:new(def, <<"stat_erl">>),
                    {ok, SData} = get_options([data, token, user_id, input_time, content], Params),
                    %% io:format("Req: ~p~n", [SData]),
                    {oid, Mb} = MongoServer:save("sources", SData),
                    %% io:format("Req: ~p~n", [Mb]),
                    equeue:add(Mb),
                    success(Req, Mb);
                _ ->
                    Req:serve_file(Path, DocRoot)
            end;
        'POST' ->
            case Path of
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.

%% Internal API
success(Req, Body) when is_binary(Body) ->
  Req:respond({200, [{"Content-Type", "text/plain"}], Body}).

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

get_options(Ks, Options) ->
    get_options(Ks, [], Options).

get_options([K|Ks], Acc, Options) ->
    Kl = atom_to_list(K),
    Rt = proplists:get_value(Kl, Options),
    case Rt of
	undefined ->
	    %% io:format("undefined, pass."),
	    get_options(Ks, Acc, proplists:delete(Kl, Options));
	Lst when is_list(Lst) ->
	    get_options(Ks, [{Kl, Lst}|Acc], proplists:delete(Kl, Options))
    end;
get_options([], Acc, _Options) ->
    {ok, Acc}.

clean_path(Path) ->
  case string:str(Path, "?") of
    0 ->
      Path;
    N ->
      string:substr(Path, 1, string:len(Path) - (N + 1))
  end.
%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
