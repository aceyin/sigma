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
%% API
-export([start_link/1, start_link/0]).
%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
start_link() -> start_link([]).
start_link(_Args) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([]) ->
  ?INFO("@@@@@@@@@@@@@@@@@"),
  ChildSpec = {
    echo,
    {echo, start_link, []},
    temporary,
    2000000,
    worker,
    [echo]
  },
  {ok, {{simple_one_for_one, 10, 10}, [ChildSpec]}}.
