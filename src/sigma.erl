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

-include("log.hrl").

%% Application callbacks
-export([start/2, stop/1]).
%% API
-export([start/0]).

%% TODO 从命令行传递进来
-define(CONF_FILE, "/Users/ace/Documents/workspace/erlang/sigma/conf/server.config").

%%--------------------------------------------------------------------
%% Application callbacks.
%%--------------------------------------------------------------------
start(Type, Args) ->
  io:format("s, args[1]:~p, args[2]:~p~n", [Type, Args]),
  {ok, Sup} = sigma_sup:start_link(),
  {ok, Sup}.

stop(_State) -> ok.

%% @doc
%% 根据配置文件中的日志配置来初始化日志系统
%% @end
init_logger() ->
  LogConfig = config:get(server_config, logger, ?DEFAULT_LOGGER),
  #{level := Level, format := Format, file := File} = LogConfig,
  logger:set_handler_config(default, level, Level),
  logger:set_handler_config(default, file, File),
  logger:update_formatter_config(default, #{single_line => true, template => Format}),
  io:format("Log config: ~p~n", [LogConfig]),
  ok.

%% @doc
%% start the server.
%% @end
start() ->
  io:format("Starting sigma at ~p ...~n", [calendar:local_time()]),
  try
    config:load(?CONF_FILE),
    init_logger(),
    application:start(?MODULE),
    start_network(),
    io:format("App sigma started at ~p~n", [calendar:local_time()]),
    ok
  catch
    _Err:Reason -> exit(Reason)
  end.

%% @doc
%% start the network listener according to the config
%% @end
start_network() ->
  case config:get(server_config, network) of
    none -> error("No network config found in game_config");
    Options -> network_app:start(Options)
  end.