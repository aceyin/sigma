%%%-------------------------------------------------------------------
%%% @author ace
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. 4月 2020 4:42 下午
%%%-------------------------------------------------------------------
-module(echo_sup).
-author("ace").

-behaviour(supervisor).
-include("logger.hrl").

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([]) ->
  ?INFO("@@@@@@@@@@@@@@@@@"),
  {ok, {{one_for_one, 10, 3600}, []}}.
