%%%-------------------------------------------------------------------
%%% @author ace
%%% @doc
%%% network_receiver 的基础封装
%%% 可以使用 -mixin([base_receiver]) 来 "继承" 本模块的函数, 从而减少重复的代码.
%%% 该 network_base 模块实现了 gen_server 所有的回调, 因此每个 network_receiver 行为
%%% 的实现者可以使用 gen_server:start_link/4 来启动.
%%% @end
%%% Created : 21. 4月 2020 10:54 下午
%%%-------------------------------------------------------------------
-module(receiver_base).
-author("ace").
-include("logger.hrl").
%% API
-export([
  start_link/1,
  init/1, take_over/2,
  handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3
]).

-define(RECEIVER_MOD, receiver_mod).

%% @doc
%% 创建一个新的 network_receiver 进程来接管刚刚新建立的客户端连接.
%% 该函数会在 network_server 进程中被调用.
%% 调用之前必须有一个新的用来处理后续网络数据的进程已经启动了.
%% @param CSock::port() 代表客户端连接的 socket 引用.
%% @param Mod::module() 即将接管 CSock 的 receiver 模块名.
%% @end
-spec(take_over(Mod :: module(), CSock :: port()) -> ok).
take_over(Mod, CSock) ->
  ?DEBUG("!!!^^^ take over socket, mod:~p", [Mod]),
  case supervisor:start_child(network_receiver_sup, []) of
    {ok, Pid} ->
      ?DEBUG("Starting child for handle network connection, child:~p", [Pid]),
      % 因 network 非法关闭时无需通知子进程,因而将子进程与 network 的监控关系改为单向的 monitor
      _MonitorRef = erlang:monitor(process, Pid),
      true = erlang:unlink(Pid),
      case gen_tcp:controlling_process(CSock, Pid) of
        ok ->
          ?DEBUG("Socket controlling process moved from network to ~p", [Pid]),
          gen_server:cast(Pid, {active, CSock}),
          {ok, Pid};
        {error, Reason} ->
          ?ERROR("Error while assign socket to process reason: ~p", [Reason]),
          {error, Reason}
      end;
    Error ->
      ?ERROR("Error handle new client connection, reason: ~p", [Error]),
      {error, Error}
  end.

start_link(Mod) ->
  ?INFO("~p server start_link called", [Mod]),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Mod], []).

%% @doc Initializes the server @end
init([Mod]) ->
  process_flag(trap_exit, true),
  % the ready function is defined in the specific receiver module.
  erlang:put(?RECEIVER_MOD, Mod),
  Mod:ready(Mod).

%% @doc
%% gen_server 的 handle_call 回调函数.
%% 注: [] 内的为可选参数.
%% 必须返回 {reply, Reply, State[,Timeout]} 或 {noreply, State[,Timeout]} 或 {stop, Reason, [Reply,] State}
%% @end
handle_call(Request, From, State) ->
  try
    Mod = erlang:get(?RECEIVER_MOD),
    Mod:on_call(Request, From, State)
  catch
    Err:Reason:Stack ->
      ?ERROR("Error while process on_call(~p,~p,~p), error:~p, reason:~p, stacktrace:~p",
             [Request, From, State, Err, Reason, Stack]),
      {reply, error, State}
  end.

%% @doc
%% gen_server 的 handle_cast 回调.
%% 注: [] 内的为可选参数.
%% 必须返回 {noreply, State} 或 {noreply, State[,Timeout]} 或 {stop, Reason, State}
%% @end
handle_cast({active, CSock}, State) ->
  async_recv(CSock, 0, -1),
  {noreply, State};
handle_cast(_Request, State) ->
  {noreply, State}.

%% @doc
%% 使用 prim_inet 的异步接口(async_accept, async_recv)时, 如果底层网络模块
%% 有收到新的连接/新的数据, 那么将会向 async_accept, async_recv 的调用者
%% (必须是一个gen_server)发送一条通知({inet_async, Socket, Ref, ...})
%% 调用者进程需要增加对应的 handle_info({inet_async,...},State) 函数
%% 用来处理异步网络数据.
%% @end
handle_info({inet_async, CSock, _Ref, {ok, Data}}, State) ->
  Mod = erlang:get(?RECEIVER_MOD),
  ?DEBUG("~~~~~~~~ Mod=~p", [Mod]),
  Mod:on_receive(Data),
  async_recv(CSock, 0, -1),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

%% @doc
%% @end
terminate(_Reason, _State) ->
  ok.

%% @doc
%% @end
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc
%% 异步接受Socket的数据.
%% @param Socket::port() 客户端socket
%% @param Len::non_neg_integer() 一次性从socket读取多少字节的数据, 0 表示全部
%% @param Timeout::integer() 读取超时时间(毫秒), -1 表示不超时.
%% @return binary() 读取到的数据.
%% @end
-spec(async_recv(Socket :: port(), Len :: non_neg_integer(), Timeout :: integer()) ->
  {ok, binary()} | {error, Reason :: term()}).
async_recv(Sock, Length, Timeout) when is_port(Sock) ->
  case prim_inet:async_recv(Sock, Length, Timeout) of
    {error, Reason} -> {error, Reason};
    {ok, Res} -> {ok, Res};
    Res -> {ok, Res}
  end;
async_recv(_Sock, _Len, _Timeout) -> {error, "param[1] is not a port"}.
