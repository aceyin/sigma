%%%-------------------------------------------------------------------
%%% @author ace
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 4月 2020 12:02 下午
%%%-------------------------------------------------------------------
-module(network_receiver_sup).
-author("ace").

-behaviour(supervisor).
-include("logger.hrl").
%% API
-export([start_link/1]).
%% Supervisor callbacks
-export([init/1]).

start_link([Mod, Fun]) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [Mod, Fun]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([Mod, Fun]) ->
  ?INFO("@@@@@@@@@@@@@@@@@"),
  ChildSpec = {
    Mod,
    {Mod, Fun, []},
    temporary,
    2000000,
    worker,
    [echo]
  },
  {ok, {{simple_one_for_one, 10, 10}, [ChildSpec]}}.