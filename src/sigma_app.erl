%%%-------------------------------------------------------------------
%% @doc sigma public API
%% @end
%%%-------------------------------------------------------------------

-module(sigma_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  application:start(config),
  Env = config:get([env, erl_max_ports]),
  io:format("####### base_dir: ~p~n", [os:getenv("base_dir")]),
  Files = filelib:wildcard("/Users/ace/Documents/workspace/erlang/sigma/cnf/*.config"),
  io:format("file list: ~p~n", [Files]),
  io:format("### all envs ~p", [Env]),
  {ok, Sup} = sigma_sup:start_link(),
  {ok, Sup}.

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
