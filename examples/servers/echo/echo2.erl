%%%-------------------------------------------------------------------
%%% @author ace
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 4月 2020 5:26 下午
%%%-------------------------------------------------------------------
-module(echo2).
-author("ace").
-include("logger.hrl").
-include_lib("network/include/network.hrl").
-mixin([receiver_base]).
-behavior(network_receiver).

%% API
-export([start_link/0, on_receive/1, ready/1]).

start_link() ->
  ?INFO("echo server start_link called"),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [?MODULE], []).

ready(Args) ->
  ?DEBUG("******* echo2:ready/1 called, ~p", [Args]),
  {ok, {}}.

%% 收到网络数据时调用
on_receive(Data) ->
  ?DEBUG("Receved data ~p", [Data]),
  ok.