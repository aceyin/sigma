%%%-------------------------------------------------------------------
%%% @author ace
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. 4月 2020 3:51 下午
%%%-------------------------------------------------------------------
-module(convert).
-author("ace").

%% API
-export([list_to_map/1]).

-spec(list_to_map(L :: list()) -> map()).
%% @doc
%% 将一个列表转化为一个 map.
%% list 中的每个元素必须是 {name, value} 的元组.
%% @end
list_to_map(List) when is_list(List) ->
  Map = #{},
  lists:foreach(List, fun({K, V}) -> maps:put(K, V, Map) end),
  Map;
list_to_map(Param) -> error(io:format("Param is not a list:~p~n", [Param])).