%%%-------------------------------------------------------------------
%%% @author ace
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 3月 2020 9:46 下午
%%%-------------------------------------------------------------------
-module(main).
-author("ace").

%% API
-export([server_start/0]).

%%% start the game server.
server_start() ->
  logger:info("Starting server ..."),
  ok.
