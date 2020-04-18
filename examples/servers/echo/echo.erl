%%%-------------------------------------------------------------------
%%% @author ace
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 一个用于测试 network 的 echo server.
%%% @end
%%% Created : 06. 4月 2020 4:45 下午
%%%-------------------------------------------------------------------
-module(echo).
-author("ace").

-behaviour(gen_server).
-include("logger.hrl").

%% API
-export([start_link/0, active/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  ?INFO("echo server start_link called"),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  ?INFO("######### 11"),
  process_flag(trap_exit, true),
  {ok, #state{}}.

active(Pid, CSock) ->
  gen_server:cast(Pid, {active, CSock}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
  ?INFO("######### 22"),
  {reply, ok, State}.

handle_cast({active, CSock}, State) ->
  async_recv(CSock, 0, -1),
  {noreply, State};
handle_cast(_Request, State) ->
  ?INFO("######### 33"),
  {noreply, State}.
handle_info({inet_async, CSock, _Ref, {ok, Data}}, State) ->
  ?INFO("Receive data from client: ~p", [Data]),
  async_recv(CSock, 0, -1),
  {noreply, State};
handle_info(_Info, State) ->
  ?INFO("######### 44"),
  {noreply, State}.


%% 接受信息
async_recv(Sock, Length, Timeout) when is_port(Sock) ->
  case prim_inet:async_recv(Sock, Length, Timeout) of
    {error, Reason} -> throw({Reason});
    {ok, Res} -> Res;
    Res -> Res
  end;
async_recv(_Sock, _Len, _Timeout) ->
  {error, "param[1] is not a port"}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ?INFO("######### 55"),
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
