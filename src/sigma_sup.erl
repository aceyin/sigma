%%%-------------------------------------------------------------------
%% @doc sigma top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(sigma_sup).

-behaviour(supervisor).
-include("logger.hrl").

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
  InitializeConfig = load_initialize_config(),
  start_child_supervisors(InitializeConfig),
  start_child_servers(InitializeConfig),
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

%% @doc 加载 initialize.config 的内容. @end
load_initialize_config() ->
  % TODO 文件路径从启动环境变量中获取
  File = "/Users/ace/Documents/workspace/erlang/sigma/src/initialize.config",
  case file:consult(File) of
    {ok, Terms} -> Terms;
    {error, Message} -> error(io:format("Error while load file initialize.config, reason:~p~n", [Message]))
  end.

%% @doc 启动 initialize.config 中配置的 supervisor. @end
-spec(start_child_supervisors(Config :: map()) -> ok).
start_child_supervisors(Config) ->
  case lists:keyfind(supervisors, 1, Config) of
    {supervisors, Sups} ->
      lists:foreach(fun do_start_sup/1, Sups);
    _ -> ok
  end.

%% do actual start a child supervisor.
-spec(do_start_sup(Spec :: map()) -> ok).
do_start_sup(Sup) ->
  {Mod, Fun, Arg} = maps:get(mfa, Sup),
  ?INFO("Starting child supervisor: ~p", [Mod]),
  Restart = maps:get(restart, Sup, permanent),
  Spec = {Mod, {Mod, Fun, Arg}, Restart, infinity, supervisor, [Mod]},
  supervisor:start_child(?MODULE, Spec).

%% @doc 启动 initialize.config 中配置的 server. @end
start_child_servers(_Config) ->
  ok.