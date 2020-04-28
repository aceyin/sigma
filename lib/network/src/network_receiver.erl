%%%-------------------------------------------------------------------
%%% @author ace
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(network_receiver).
-include("logger.hrl").

-callback(on_receive(Data :: any()) -> {ok, term()}).
-callback(ready(Args :: term()) -> {ok, State :: term()}).
-optional_callbacks([ready/1]).
