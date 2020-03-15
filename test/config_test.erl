%%%-------------------------------------------------------------------
%%% @author ace
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. 3月 2020 11:04 下午
%%%-------------------------------------------------------------------
-module(config_test).
-author("ace").

-include_lib("eunit/include/eunit.hrl").

setup() ->
  Cnf = #{
    atom_key => 32000,
    "string_key" => 500000,
    inner_map => #{
      hello => ok
    }
  },
  application:set_env(sigma, env, Cnf),
  ok.

get_test() ->
  setup(),
  io:format("all config :~p~n", [application:get_env(sigma, env)]),
  Val = config:get([env, atom_key]),
  io:format("Val :~p~n", [Val]),
  ?assertEqual(Val, 32000),
  Val2 = config:get([env, "string_key"]),
  io:format("Val :~p~n", [Val2]),
  ?assertEqual(Val2, 500000),
  Val3 = config:get([env, inner_map, hello]),
  io:format("Val :~p~n", [Val3]),
  ?assertEqual(Val3, ok),
  ok.

keys_test() ->
  Keys = config:names(),
  io:format("keys =~p~n", [Keys]),
  Expect = [env],
  ?assertEqual(Expect, Keys).
