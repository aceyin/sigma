%%%-------------------------------------------------------------------
%%% @author ace
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. 3月 2020 11:37 下午
%%%-------------------------------------------------------------------
-module(network).
-author("ace").
-behaviour(gen_server).
-include("network.hrl").

%% API
-export([start_link/1, stop/0, set_max_conn/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Config) ->
  ?INFO("Starting network server"),
  gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%% @doc stop network server. @end
stop() ->
  ?INFO("Stopping network server"),
  gen_server:cast(?MODULE, stop).

%% @doc set the max allowed connections programmatically. @end
set_max_conn(N) -> gen_server:cast(?MODULE, {set_max_conn, N}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Config) ->
  ?DEBUG("Initializing network module with config:~p", [Config]),
  erlang:process_flag(trap_exit, true),
  erlang:process_flag(priority, high),
  #net_config{max_conn = Max, receiver = Receiver, port = Port, options = Options} = Config,
  ServerSocket = start_listen(Port, Options),
  % start accept network connection
  gen_server:cast(self(), accept),
  {ok, #net_state{server_socket = ServerSocket, max = Max, receiver = Receiver}}.

%% @doc do set max allowed connections of the network server. @end
handle_call({set_max_conn, N}, _From, State) ->
  NewState = State#net_state{max = N},
  {reply, ok, NewState};
handle_call(Request, From, State) ->
  ?WARNING("network server received unknown call request from ~p: ~p", [From, Request]),
  {noreply, State}.

%% @doc
%% 开始接受下一次网络请求.
%% 因为网络服务器的模式设置为{active, false}, 因此每次在处理完新进来的网络连接之后,
%% 必须再手工调用一次本函数, 来让网络服务器能继续接收下一次请求.
%% @end
handle_cast(accept, State) ->
  accept(State);
handle_cast(_Request, State) ->
  ?WARNING("network app received unknown cast requst: ~p", [_Request]),
  {noreply, State}.

%% @doc
%% OTP 异步网络模块(prim_inet:async_accept)在收到客户端连接之后, 会通过消息将链接发送到监听者进程.
%% 这一系列 handle_info({inet_xxx) 相关的函数就是用来接收网卡模块发送的连接信息的.
%% @end
handle_info({inet_async, SSock, Ref, {ok, CSock}},
            State = #net_state{server_socket = SSock, ref = Ref, count = Count, receiver = Rcv}) ->
  true = inet_db:register_socket(CSock, inet_tcp),
  ?DEBUG("Incoming client ServerSock:~p, Ref:~p, Sock:~p, State:~p", [SSock, Ref, CSock, State]),
  % 为每个新建立的客户端启动一个新的进程, 用来处理新的网络连接, 并将tcp控制权交给此进程
  #{sup := Sup, mod := _Mod} = Rcv,
  NewState =
  case supervisor:start_child(Sup, []) of
    {ok, Child} ->
      ?DEBUG("Starting child for handle network connection, child:~p", [Child]),
      % 因 network 非法关闭时无需通知子进程,因而将子进程与 network 的监控关系改为单向的 monitor
      _MonitorRef = erlang:monitor(process, Child),
      true = erlang:unlink(Child),
      case gen_tcp:controlling_process(CSock, Child) of
        ok ->
          ?DEBUG("Socket controlling process moved from network to ~p", [Child]),
          State#net_state{count = Count + 1};
        {error, Reason} ->
          ?ERROR("Error while assign socket to process ~p, reason: ~p", [Sup, Reason]),
          gen_tcp:close(CSock),
          State
      end;
    Error ->
      ?ERROR("Error handle new client connection: failed to start proceess: ~p, reason: ~p", [Sup, Error]),
      gen_tcp:close(CSock),
      State
  end,
  accept(NewState);
%% @doc 处理客户端连接关闭时的回调 @end
handle_info({inet_async, _SSock, Ref, {error, closed}}, State = #net_state{count = Count}) ->
  NewState = State#net_state{count = Count - 1},
  ?INFO("Client socket closed, ref: ~p, state:~p", [Ref, NewState]),
  {stop, normal, NewState};
handle_info({inet_async, _SSock, Ref, Error}, State) ->
  ?ERROR("Accept new client connection error, ref:~p, reason:~p", [Ref, Error]),
  accept(State);
%% @doc 处理 acceptor 子进程退出的回调 @end
handle_info({'DOWN', _Ref, process, Pid, Info}, State = #net_state{count = Count}) ->
  ?INFO("Client process ~p exited, reason:~p", [Pid, Info]),
  {noreply, State#net_state{count = Count - 1}};
handle_info(_Info, State) ->
  {noreply, State}.
%% @doc 处理 network app 终止时的回调 @end
terminate(Reason, #net_state{server_socket = Socket}) ->
  ?INFO("network app receive terminate for reason ~p", [Reason]),
  case erlang:is_port(Socket) of
    true -> close_socket(Socket);
    false -> ok
  end,
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc 将配置文件中的TCP选项与默认的合并, 生成用于启动网络模块的TCP选项 @end
merge_options(Options) ->
  Map = maps:from_list(Options),
  [begin
     case Item of
       binary -> binary;
       {K, _} ->
         case maps:get(K, Map, none) of
           none -> Item;
           V -> {K, V}
         end
     end
   end || Item <- ?DEFAULT_OPTIONS
  ].

-spec(start_listen(Port :: non_neg_integer(), Options :: list()) -> port()).
%% @doc start TCP socket listen. @end
start_listen(Port, Options) ->
  NewOptions = merge_options(Options),
  case gen_tcp:listen(Port, NewOptions) of
    {ok, Socket} ->
      ?INFO("network app listen on port ~p with options: ~p", [Port, NewOptions]),
      Socket;
    {error, Reason} ->
      ?ERROR("Error while open socket on port ~p, reason:~p", [Port, Reason]),
      exit(Reason)
  end.

%% @doc start to accept client connection. @end
accept(State = #net_state{server_socket = ServerSocket}) ->
  ?INFO("network app ready for accepting connections"),
  case prim_inet:async_accept(ServerSocket, -1) of
    {ok, Ref} ->
      ?DEBUG("Accept on server socket ~p, Ref:~p", [ServerSocket, Ref]),
      {noreply, State#net_state{ref = Ref}};
    Error ->
      ?ERROR("Error while accept client connection, reason:~p", [Error]),
      {stop, {cannot_accept, Error}, State}
  end.

%% @doc close the socket.
close_socket(Port) ->
  unlink(Port),
  catch erlang:port_close(Port),
  receive {tcp_closed, Port} -> ok after 0 -> ok end.