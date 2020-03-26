%%%-------------------------------------------------------------------
%%% @author ace
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 3月 2020 11:30 下午
%%%-------------------------------------------------------------------
-module(network_app).
-author("ace").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
%% API functions.
-export([start/1, stop/0]).

%% @doc the application start callback.
-spec(start(Type :: normal | {takeover, node()} | {failover, node()}, Args :: term()) ->
  {ok, pid()} | {ok, pid(), State :: term()} | {error, Reason :: term()}).
start(_Type, _Args) ->
  {ok, Options} = application:get_env(network, network_options),
  case network_sup:start_link(Options) of
    {ok, Sup} -> {ok, Sup};
    Error -> Error
  end.

%% @doc the application stop callback.
-spec(stop(State :: term()) -> term()).
stop(_State) -> ok.

start(Args) ->
  application:set_env(network, network_options, Args),
  application:start(network).

stop() -> application:stop(network).

