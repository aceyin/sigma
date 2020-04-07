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
  {ok, Sup} = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
  % TODO 将系统启动时需同时启动的模块放到配置里面或者封装成自动加载的方式
  Spec = {
    echo_sup,
    {echo_sup, start_link, []},
    permanent, infinity, supervisor, [echo_sup]
  },
  supervisor:start_child(?MODULE, Spec),
  {ok, Sup}.

%%====================================================================
%% Supervisor callbacks
%%====================================================================
init([]) ->
  io:format("sigma_sup:init/1 called ~n"),
  {ok, {{one_for_all, 0, 1}, []}}.

%%====================================================================
%% Internal functions
%%====================================================================
