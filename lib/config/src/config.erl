-module(config).

%% API exports
-export([get/1, get/2, keys/1, load/1]).

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
%%    mysql => #{
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
%%   mysql => #{
%%     host => "10.3.0.1",
%%     port => 3306,
%%     user => "root",
%%     pass => "123456"
%%   }
%% }
%% ```
%% config:get([database, mysql, host]) will return "10.3.0.1"
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
-spec keys(atom()) -> [atom()].
keys(Name) -> Name:all_keys_of_it().

%% @doc
%% Dynamic load a config file into system config.
%% @end
-spec load(string()) -> ok | {error, string()}.
load(FilePath) ->
  case filelib:is_file(FilePath) of
    false -> {error, io:format("~p is not a file.", [FilePath])};
    true ->
      BaseName = filename:basename(FilePath),
      case file:consult(FilePath) of
        {ok, TermList} ->
          Mod = list_to_atom(filename:rootname(BaseName)),
          Bin = compiler:compile(Mod, TermList),
          code:purge(Mod),
          {module, Mod} = code:load_binary(Mod, atom_to_list(Mod) ++ ".erl", Bin),
          ok;
        Error ->
          {error, io:format("Error while parse config file ~p, reason: ~p~n", [BaseName, Error])}
      end
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