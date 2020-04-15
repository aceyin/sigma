%%%-------------------------------------------------------------------
%%% @author ace
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 3月 2020 11:30 下午
%%%-------------------------------------------------------------------
-module(network_sup).
-author("ace").
-include("logger.hrl").

-behaviour(supervisor).
%% API
-export([start_link/1]).
%% Supervisor callbacks
-export([init/1]).

start_link([]) ->
  ?DEBUG("network_sup:start_link/1 called"),
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc supervisor callback.
init(_) ->
  ?DEBUG("network_sup:init/1 called"),
  % Defines the function call used to start the child process.
  % It must be a module-function-arguments tuple {M,F,A} used as apply(M,F,A).
  Start = {network_server, start_link, []},
  % Defines when a terminated child process must be restarted.
  % permanent: child process is always restarted.
  % temporary: child process is never restarted.
  % transient: child process is restarted only if it terminates abnormally,
  %            that is, with another exit reason than normal, shutdown, or {shutdown,Term}.
  Restart = permanent,
  % The supervisor tells the child process to terminate by calling exit(Child,shutdown)
  % and then wait for an exit signal with reason shutdown back from the child process.
  % If no exit signal is received within the specified number of milliseconds,
  % the child process is unconditionally terminated using exit(Child,kill).
  Shutdown = 20000,
  Type = worker,
  % Used by the release handler during code replacement to determine which processes
  % are using a certain module.
  Modules = [network_server],
  %  ChildSpec = {network, Start, Restart, Shutdown, Type, Modules},
  ChildSpec = #{
    id => network_server,
    start => Start,
    restart => Restart,
    shutdown => Shutdown,
    type => Type,
    modules => Modules
  },
  % one_for_one:
  %     If one child process terminates and is to be restarted, only that child process is affected.
  % one_for_all:
  %     If one child process terminates and is to be restarted, all other child processes are
  %     terminated and then all child processes are restarted.
  % rest_for_one:
  %     If one child process terminates and is to be restarted, the 'rest' of the child processes
  %     (that is, the child processes after the terminated child process in the start order)
  %     are terminated.
  % simple_one_for_one:
  %     A simplified one_for_one supervisor, where all child processes are dynamically added
  %     instances of the same process type, that is, running the same code.
  RestartStrategy = one_for_one,
  MaxRestarts = 10,
  MaxSecondsBetweenRestarts = 10,
  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
  {ok, {SupFlags, [ChildSpec]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
