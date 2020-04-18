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
-include("network.hrl").
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% @doc the application start callback. @end
start(_Type, _Args) ->
  ?DEBUG("network_app:start/2 called"),
  case network_sup:start_link([]) of
    {ok, Sup} ->
      % start network receiver sup
      case application:get_env(network, config) of
        undefined -> error("No network config found in sigma_config");
        {ok, Map} ->
          ?DEBUG("Starting network server with config: ~p", [Map]),
          #{receiver := {Mod, Fun}} = Map,
          {ok, _Sup2} = network_receiver_sup:start_link([Mod, Fun])
      end,
      {ok, Sup};
    Error -> Error
  end.

%% @doc the application stop callback.
-spec(stop(State :: term()) -> term()).
stop(_State) -> ok.


