-module(config).

%% API exports
-export([get/1, get/2, names/0]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc
%% Get the value of a config item, return none if no such config item found.
%% @end
-spec get(atom()|[atom()]) -> any().
get(Name) -> get(Name, none).

%% @doc
%% Get the value of a config item, return the Default value if no item found.
%% The Name can be an atom of an atom list; if it is a atom.
%% For example:
%% Give a config file with the content:
%% ```
%% [{sigma, [
%%  {database, #{
%%    cn => #{
%%      host => "10.3.0.1",
%%      port => 3306,
%%      user => "root",
%%      pass => "123456"
%%    }
%%  }},
%% ]
%% }]
%% ```
%% config:get(database) will return a map:
%% ```
%% #{
%%   cn => #{
%%     host => "10.3.0.1",
%%     port => 3306,
%%     user => "root",
%%     pass => "123456"
%%   }
%% }
%% ```
%% config:get([database, cn, host]) will return "10.3.0.1"
%% @end
-spec get(atom() | [atom()], any()) -> any().
get(Name, Default) when not is_list(Name) ->
  io:format("1 get, Name=~p~n", [Name]),
  case application:get_env(?MODULE, Name) of
    undefined -> Default;
    {ok, Ret} -> Ret
  end;
get([Name], Default) ->
  io:format("2 get, Name=~p~n", [Name]),
  case application:get_env(?MODULE, Name) of
    undefined -> Default;
    {ok, Ret} -> Ret
  end;
get([Name | Keys], Default) ->
  io:format("3 get, Name=~p~n", [Name]),
  case application:get_env(?MODULE, Name) of
    undefined -> Default;
    {ok, Map} -> get_map_item(Map, Keys, Default)
  end.

%% @doc
%% Set the config item to the given value.
%% If the config already exists, replace it;
%% otherwise create a new one.
%% @end
-spec set(atom()|[atom()], any()) -> ok.
set(Name, Value) when is_atom(Name) ->
  application:set_env(?MODULE, Name, Value),
  ok.

%% @doc
%% Get all the keys of the application config.
%% @end
-spec names() -> [atom()].
names() ->
  All = application:get_all_env(sigma),
  List = [Key || {Key, _} <- All],
  List.

%% @doc
%% Dynamic load a config file into system config.
%% @end
-spec load(string()) -> ok | {error, string()}.
load(FileName) ->
  case filelib:is_file(FileName) of
    false -> {error, io:format("~p is not a file.", [FileName])};
    true ->
      file:consult(FileName)
  end.

%%====================================================================
%% Internal functions
%%====================================================================
get_map_item(Map, [Key], Default) when is_map(Map) ->
  case maps:find(Key, Map) of
    {ok, Val} -> Val;
    error -> Default
  end;
get_map_item(Map, [Key | Rest], Default) when is_map(Map) ->
  case maps:find(Key, Map) of
    {ok, Val} -> get_map_item(Val, Rest, Default);
    error -> Default
  end;
get_map_item(Val, _, Default) when not is_map(Val) ->
  Default.