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

-include("logger.hrl").

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
  ?DEBUG("sigma start/2 called, args[1]:~p, args[2]:~p~n", [Type, Args]),
  {ok, Sup} = sigma_sup:start_link(),
  {ok, Sup}.

stop(_State) -> ok.

%% @doc
%% start the server.
%% @end
start() ->
  ?INFO("Starting sigma at ~p ...", [calendar:local_time()]),
  try
    config:load(?CONF_FILE),
    initializer:init_logger(),
    application:start(?MODULE),
    start_network(),
    ?INFO("App sigma started at ~p", [calendar:local_time()]),
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