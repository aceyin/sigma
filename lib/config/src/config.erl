-module(config).

%% API exports
-export([get/3, get/2, keys/1, load/1]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc
%% Get the value of a config item, return none if no such config item found.
%% Name the name of config file.
%% @end
-spec get(atom(), atom()) -> any().
get(Name, Key) -> get(Name, Key, none).

%% @doc
%% Get the value of a config item, return the Default value if no item found.
%% @end
-spec get(atom(), atom(), any()) -> any().
get(Name, Key, Default) ->
  try
    Name:Key()
  catch
    _:_ -> Default
  end.

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