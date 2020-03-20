%%%------------------------------------------------------
%%% @author ace
%%% @doc
%%% Inspired by "mochiglobal.erl"
%%% <a href="https://github.com/mochi/mochiweb/blob/master/src/mochiglobal.erl">mochiglobal</a>
%%% Abuse module constant pools as a "read-only shared heap" (since erts 5.6)
%%% <a href="http://www.erlang.org/pipermail/erlang-questions/2009-March/042503.html">[1]</a>.
%%% @end
-module(compiler).
-author("Ace Yin <ync@163.com>").
-export([compile/2]).
-define(E_ATTR, erl_syntax:attribute).
-define(E_ATOM, erl_syntax:atom).
-define(E_LIST, erl_syntax:list).
-define(E_ARITY, erl_syntax:arity_qualifier).
-define(E_INT, erl_syntax:integer).
-define(E_FUN, erl_syntax:function).
-define(E_CLS, erl_syntax:clause).
-define(E_ABST, erl_syntax:abstract).

-spec compile(atom(), any()) -> binary().
compile(Module, Terms) ->
  T = compile:forms(forms(Module, Terms), [verbose, report_errors]),
  {ok, Module, Bin} = T,
  Bin.

-spec forms(atom(), [term()]) -> [erl_syntax:syntaxTree()].
forms(Module, Terms) ->
  [erl_syntax:revert(X) || X <- term_to_abstract(Module, Terms)].

-spec term_to_abstract(atom(), any()) -> [erl_syntax:syntaxTree()].
term_to_abstract(Module, Terms) ->
  %% -module(Module).
  ModuleDefine = ?E_ATTR(?E_ATOM(module), [?E_ATOM(Module)]),
  %% -export([Key/0]).
  Exports = [?E_ATTR(?E_ATOM(export), [?E_LIST([?E_ARITY(?E_ATOM(Key), ?E_INT(0))])]) || {Key, _} <- Terms],
  %% -export([all_keys_of_it/0])
  AllKeysOf = ?E_ATTR(?E_ATOM(export), [?E_LIST([?E_ARITY(?E_ATOM(all_keys_of_it), ?E_INT(0))])]),
  %% functions
  AllFun = [?E_FUN(?E_ATOM(Key), [?E_CLS([], none, [?E_ABST(T)])]) || {Key, T} <- Terms],
  %% all_keys_of_() -> List.
  ListFun = ?E_FUN(?E_ATOM(all_keys_of_it), [?E_CLS([], none, [?E_LIST([?E_ATOM(Key) || {Key, _} <- Terms])])]),
  lists:flatten([ModuleDefine, Exports, AllKeysOf, AllFun, ListFun]).
