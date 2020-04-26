%%%-------------------------------------------------------------------
%%% @author ace
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(echo2).
-include("logger.hrl").
-include_lib("network/include/network.hrl").
-mixin([receiver_base]).
-behavior(network_receiver).

%% API
-export([ready/1, on_receive/1]).

-spec ready(Args :: term()) -> {ok, State :: term()}.
ready(Args) ->
  ?DEBUG("******* echo2:ready/1 called, ~p", [Args]),
  {ok, {}}.

-spec on_receive(Args :: term()) -> {State :: term()}.
on_receive(Data) ->
  ?DEBUG("Receved data ~p", [Data]),
  {}.