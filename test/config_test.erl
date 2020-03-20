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

load_test() ->
  File = "/Users/ace/Documents/workspace/erlang/sigma/test/sample.config",
  Loaded = config:load(File),
  ?assertEqual(ok, Loaded),
  ok.

get_test() ->
  Simple = config:get(sample, simple_conf),
  % simple data
  ?assertEqual(100, Simple),
  % map data
  Map = config:get(sample, map_conf),
  ?assertEqual(1, maps:get(id, Map)),
  ?assertEqual("sigma_server", maps:get(name, Map)),
  ?assertEqual([1, 2, 3], maps:get(cookie, Map)),
  io:format("map data ~p~n", [Map]),
  % list data
  List = config:get(sample, list_conf),
  ?assertEqual([1, "string", #{key => value}], List),
  io:format("list data ~p~n", [List]),
  ok.

get_with_default_test() ->
  NoExist = config:get(non_exist_name, no_exist_key, "default"),
  ?assertEqual(NoExist, "default"),
  ok.


