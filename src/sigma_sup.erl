%%%-------------------------------------------------------------------
%% @doc sigma top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(sigma_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  io:format("sigma_sup:start_link/0 called ~n"),
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
init([]) ->
  io:format("sigma_sup:init/1 called ~n"),
  {ok, {{one_for_all, 0, 1}, []}}.

%%====================================================================
%% Internal functions
%%====================================================================
