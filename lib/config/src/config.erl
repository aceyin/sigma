-module(config).

%% API exports
-export([get/3, get/2, all/1, load/1]).

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
  try Name:Key()
  catch _:_ -> Default
  end.

%% @doc
%% Get all the keys of the application config.
%% @end
-spec all(atom()) -> [atom()].
all(Name) -> Name:all_keys_of_it().


%% @doc
%% Dynamic load a config file into system config.
%% @end
-spec load(string()) -> ok | {error, string()}.
load(FilePath) ->
  case filelib:is_file(FilePath) of
    false -> error(io:format("~p is not a file.", [FilePath]));
    true ->
      BaseName = string:lowercase(filename:basename(FilePath)),
      RootName = filename:rootname(BaseName),
      Extension = string:slice(string:lowercase(filename:extension(FilePath)), 1),
      ModName = case Extension of
                  [] -> RootName;
                  "erl" -> RootName;
                  _ -> string:join([RootName, Extension], "_")
                end,
      case file:consult(FilePath) of
        {ok, TermList} ->
          Mod = list_to_atom(ModName),
          Bin = compiler:compile(Mod, TermList),
          code:purge(Mod),
          {module, Mod} = code:load_binary(Mod, atom_to_list(Mod) ++ ".erl", Bin),
          io:format("Compile config file ~p into module ~p success~n", [FilePath, Mod]),
          io:format("All config items in module ~p are: ~p~n ", [Mod, config:all(Mod)]),
          ok;
        Error ->
          error(io:format("Error while parse config file ~p, reason: ~p", [BaseName, Error]))
      end
  end.