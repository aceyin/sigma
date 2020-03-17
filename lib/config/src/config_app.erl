%%%-------------------------------------------------------------------
%% @doc config public API
%% @end
%%%-------------------------------------------------------------------

-module(config_app).
-behaviour(application).
-include("keys.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================
start(_StartType, _StartArgs) ->
  io:format("@@@@@@@@@@@@@@@@"),
  {ok, Sup} = config_sup:start_link(),
  BaseDir = os:getenv(?BASE_DIR),
  io:format("base_dir is:~p~n", [BaseDir]),
%%  Files = file:list_dir(),
%%  {ok, Sup}.
  error.
%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
