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
-export([start_link/0, stop/0, set_max_conn/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
  ?INFO("Starting network server"),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc stop network server. @end
stop() ->
  ?INFO("Stopping network server"),
  gen_server:cast(?MODULE, stop).

%% @doc set the max allowed connections programmatically. @end
set_max_conn(N) -> gen_server:cast(?MODULE, {set_max_conn, N}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec(init(Config :: #net_config{}) ->
  {ok, #net_state{}} | {ok, #net_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init(Config) ->
  ?DEBUG("Initializing network module with config:~p", [Config]),
  erlang:process_flag(trap_exit, true),
  erlang:process_flag(priority, high),
  ServerSocket = start_listen(Config),
  #net_config{max_conn = Max} = Config,
  gen_server:cast(self(), accept),
  {ok, #net_state{server_socket = ServerSocket, max = Max, config = Config}}.

%% @doc do set max allowed connections of the network server. @end
handle_call({set_max_conn, N}, _From, State) ->
  NewState = State#net_state{max = N},
  {reply, ok, NewState};
handle_call(Request, From, State) ->
  ?WARNING("network server received unknown call request from ~p: ~p", [From, Request]),
  {noreply, State}.

-spec(handle_cast(Request :: term(), State :: #net_state{}) ->
  {noreply, NewState :: #net_state{}} |
  {noreply, NewState :: #net_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #net_state{}}).
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
handle_info({inet_async, ServerSock, Ref, {ok, ClientSocket}},
            State = #net_state{server_socket = ServerSock, ref = Ref, count = Count}) ->
  true = inet_db:register_socket(ClientSocket, inet_tcp),
  ?DEBUG("Incoming client ServerSock:~p, Ref:~p, Sock:~p, State:~p", [ServerSock, Ref, ClientSocket, State]),
  %% TODO 启动一个新的玩家进程, 并将tcp控制权交给此进程
  accept(State#net_state{count = Count + 1});
handle_info({inet_async, LSock, Ref, {error, closed}}, State) ->
  ?INFO("Socket closed, LSock:~p, Ref:~p", [LSock, Ref]),
  {stop, normal, State};
handle_info({inet_async, LSock, Ref, Error}, State) ->
  ?ERROR("Accept error, LSock:~p, Ref:~p, Error:~p", [LSock, Ref, Error]),
  accept(State);
handle_info({'DOWN', _Ref, process, Pid, Info}, State = #net_state{count = Count}) ->
  ?INFO("Client process ~p closed connection, reason:~p", [Pid, Info]),
  {noreply, State#net_state{count = Count - 1}};
handle_info(_Info, State) ->
  {noreply, State}.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #net_state{}) -> term()).
terminate(_Reason, #net_state{server_socket = Socket}) ->
  ?INFO("network app receive terminate for reason ~p", [_Reason]),
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

-spec(start_listen(Config :: #net_config{}) -> port()).
%% @doc start TCP socket listen. @end
start_listen(Config) ->
  #net_config{port = Port, options = Options} = Config,
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