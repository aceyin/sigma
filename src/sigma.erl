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
  io:format("sigma start/2 called, args[1]:~p, args[2]:~p~n", [Type, Args]),
  {ok, Sup} = sigma_sup:start_link(),
  {ok, Sup}.

stop(_State) -> ok.

%% @doc
%% start the server.
%% @end
start() ->
  try
    config:load(?CONF_FILE),
    application:start(?MODULE),
    start_network(),
    ok
  catch
    _Err:Reason -> exit(Reason)
  end.

%% @doc
%% start the network listener according to the config
%% @end
start_network() ->
  network_app:start([]),
  ok.