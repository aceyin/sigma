%%%-------------------------------------------------------------------
%%% @author ace
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 3月 2020 9:46 下午
%%%-------------------------------------------------------------------
-module(sigma).
-behaviour(application).
-author("ace").

-include("sigma.hrl").
-include_lib("network/include/network.hrl").

%% Application callbacks
-export([start/2, stop/1]).
%% API
-export([start/0, stop/0]).

%% TODO 从命令行传递进来
-define(CONF_FILE, "/Users/ace/Documents/workspace/erlang/sigma/conf/sigma.config").


%%--------------------------------------------------------------------
%% API.
%%--------------------------------------------------------------------

%% @doc start the server. @end
start() ->
  try
    load_config(),
    setup_logger(),
    ?INFO("******************* STARTING SIGMA [~p] *******************", [calendar:local_time()]),
    % ensure all dependent app started
    application:ensure_all_started(sasl),
    application:start(?MODULE),
    ?INFO("******************* SIGMA STARTED [~p] *******************", [calendar:local_time()]),
    ok
  catch
    Type:Error:Stack ->
      io:format("******************* SIGMA START FAILED *******************~n~p:~p~n~p~n",
                [Type, Error, Stack]),
      init:stop(-1)
  end.

%% @doc stop the server app. @end
stop() -> application:stop(?SIGMA).

%%--------------------------------------------------------------------
%% Application callbacks.
%%--------------------------------------------------------------------
start(Type, Args) ->
  ?DEBUG("sigma start/2 called, args[1]:~p, args[2]:~p~n", [Type, Args]),
  {ok, Sup} = sigma_sup:start_link(),
  {ok, Sup}.

stop(_State) -> ok.

%% ===================================================================
%% INTERNAL FUNCTIONS
%% ===================================================================

%% @doc 加载服务配置文件, 并将其与 application:get_env 绑定 @end
load_config() ->
  % 加载配置文件
  config:load(?CONF_FILE),
  All = config:all(?SIGMA_CONFIG),
  Fun = fun(Key) -> application:set_env(Key, config, config:get(?SIGMA_CONFIG, Key)) end,
  lists:foreach(Fun, All),
  ok.

%% @doc 用 sigma.config 里面配置的参数初始化日志系统. @end
setup_logger() ->
  LogConfig = config:get(?SIGMA_CONFIG, logger, ?DEFAULT_LOGGER),
  #{level := Level, format := Format, file := File} = LogConfig,
  logger:set_handler_config(default, level, Level),
  logger:set_handler_config(default, file, File),
  logger:update_formatter_config(default, #{
    time_offset=>"Z",
    time_designator=>$\s,
    single_line => true,
    template => Format
  }),
  ?INFO("Use logger config: ~p", [LogConfig]),
  ok.
