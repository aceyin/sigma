%%%-------------------------------------------------------------------
%%% @author ace
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 3月 2020 9:46 下午
%%%-------------------------------------------------------------------
-module(sigma).
-author("ace").

%% API
-export([start/0]).

%%% start the game server.
start() ->
  io:format("sigma start/0 called"),
  network_app:start([]),
  ok.
