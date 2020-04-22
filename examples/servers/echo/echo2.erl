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
-mixin([network_receiver]).

-record(state, {}).

-behavior(network_receiver).

%% API
-export([start_link/0, ready/1, on_receive/1]).

start_link() ->
  ?INFO("echo server start_link called"),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

ready(Args) ->
  ?DEBUG("echo 2 ready called, args: ~p", [Args]),
  {ok, #state{}}.

%% 收到网络数据时调用
on_receive(Data) ->
  ?DEBUG("Receved data ~p", [Data]),
  ok.

on_call()->ok.
on_cast()->ok.
on_info()->ok.