%%%-------------------------------------------------------------------
%%% @author ace
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 4月 2020 7:38 下午
%%%-------------------------------------------------------------------
-module(initializer).
-author("ace").
-include("logger.hrl").

%% API
-export([init_logger/0]).

%% @doc
%% 用 server.config 里面配置的参数初始化日志系统.
%% @end
init_logger() ->
  LogConfig = config:get(server_config, logger, ?DEFAULT_LOGGER),
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