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

-include("cores.hrl").
-include_lib("network/include/network.hrl").

%% Application callbacks
-export([start/2, stop/1]).
%% API
-export([start/0, stop/0]).

%% TODO 从命令行传递进来
-define(CONF_FILE, "/Users/ace/Documents/workspace/erlang/sigma/conf/server.config").


%%--------------------------------------------------------------------
%% API.
%%--------------------------------------------------------------------

%% @doc start the server. @end
start() ->
  try
    config:load(?CONF_FILE),
    init_logger(),
    ?INFO("******************* starting sigma [~p] *******************~n", [calendar:local_time()]),
    % ensure all dependent app started
    application:ensure_all_started(sasl),
    start_network(),
    application:start(?MODULE),
    ?INFO("******************* sigma started [~p] *******************", [calendar:local_time()]),
    ok
  catch
    _Type:_Error ->
      io:format("******************* SERVER START FAILED *******************~n~p:~p~n~p~n",
                [_Type, _Error, erlang:get_stacktrace()]),
      init:stop(-1)
  end.


%% @doc
%% start the network listener according to the config
%% @end
start_network() ->
  case config:get(server_config, network) of
    none -> error("No network config found in server_config");
    Map ->
      ?DEBUG("Starting network app with config: ~p", [Map]),
      #{options := Options, port := Port, max_conn := Max, receiver := Receiver} = Map,
      network_app:start(#net_config{
        options = Options, port = Port, max_conn = Max, receiver = Receiver
      })
  end.

stop() -> application:stop(sigma).

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

%% @doc 用 server.config 里面配置的参数初始化日志系统. @end
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