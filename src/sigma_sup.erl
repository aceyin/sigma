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
  Bootstrap = load_bootstrap_config(),
  start_child_supervisors(Bootstrap),
  start_child_applications(Bootstrap),
  start_child_servers(Bootstrap),
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

%% @doc 加载 bootstrap.config 的内容. @end
load_bootstrap_config() ->
  % TODO 文件路径从启动环境变量中获取
  File = "/Users/ace/Documents/workspace/erlang/sigma/src/bootstrap.config",
  case file:consult(File) of
    {ok, Terms} -> Terms;
    {error, Message} -> error(io:format("Error while load file bootstrap.config, reason:~p~n", [Message]))
  end.

%% @doc 启动 bootstrap.config 中配置的 supervisor. @end
-spec(start_child_supervisors(Config :: map()) -> ok).
start_child_supervisors(Config) ->
  case lists:keyfind(supervisors, 1, Config) of
    {supervisors, Sups} ->
      lists:foreach(fun do_start_sup/1, Sups);
    false -> ok
  end.

%% @doc 启动 bootstrap.config 中的 apps. @end
-spec(start_child_applications(Config :: map()) -> ok).
start_child_applications(Config) ->
  case lists:keyfind(apps, 1, Config) of
    {apps, Apps} ->
      lists:foreach(fun do_start_app/1, Apps);
    false -> ok
  end.

%% @doc 启动 bootstrap.config 中配置的 server. @end
start_child_servers(_Config) ->
  ok.

%% do actual start a child supervisor.
-spec(do_start_sup(Spec :: map()) -> ok).
do_start_sup(Sup) ->
  {Mod, Fun, Arg} = maps:get(mfa, Sup),
  ?INFO("Starting child supervisor: ~p", [Mod]),
  Restart = maps:get(restart, Sup, permanent),
  Spec = {Mod, {Mod, Fun, Arg}, Restart, infinity, supervisor, [Mod]},
  supervisor:start_child(?MODULE, Spec).

do_start_app({Name}) -> do_start_app({Name, temporary});
do_start_app({Name, Type}) -> application:start(Name, Type).