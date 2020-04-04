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

-record(state, {
  % the server socket
  socket = 0 :: port(),
  % current used socket options.
  options :: #socket_option{},
  % client count
  count = 0 :: non_neg_integer(),
  % max connection
  max,
  % current client socket ref
  ref
}).

-define(DEFAULT_PORT, 8971).

-define(TCP_OPTIONS, [
  binary,
  {active, false},
  {reuseaddr, true},
  {delay_send, true},
  {nodelay, true},
  {send_timeout, 8000},
  {exit_on_close, false}
]).

%%%===================================================================
%%% API
%%%===================================================================

-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  ?DEBUG("network start_link/0 called"),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc stop network server.
stop() -> gen_server:cast(?MODULE, stop).

set_max_conn(N) -> gen_server:cast(?MODULE, {set_max_conn, N}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
  {ok, #state{}} | {ok, #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init(_Args) ->
  ?DEBUG("network init/1 called~n"),
  erlang:process_flag(trap_exit, true),
  erlang:process_flag(priority, high),
  Socket = start_listen(),
  Max = 1000, %% TODO get from config file
  gen_server:cast(self(), accept),
  {ok, #state{socket = Socket, max = Max}}.

-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #state{}) ->
                   {reply, Reply :: term(), NewState :: #state{}} |
                   {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
                   {noreply, NewState :: #state{}} |
                   {noreply, NewState :: #state{}, timeout() | hibernate} |
                   {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
                   {stop, Reason :: term(), NewState :: #state{}}).
handle_call({set_max_conn, N}, _From, State) ->
  NewState = State#state{max = N},
  {reply, ok, NewState};
handle_call(_Request, _From, State) ->
  ?WARNING("network app received unknown call request ~p", [_Request]),
  {noreply, State}.

%% @doc start to accept network connection.
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(accept, State) ->
  accept(State);
handle_cast(_Request, State) ->
  ?WARNING("network app received unknown cast requst: ~p", [_Request]),
  {noreply, State}.

%% @doc
%% OTP 异步网络模块(prim_inet:async_accept)在收到客户端连接之后, 会通过消息将链接发送到监听者进程.
%% 这一系列 handle_info({inet_xxx) 相关的函数就是用来接收网卡模块发送的连接信息的.
%% @end
handle_info({inet_async, LSock, _Ref, {ok, Sock}}, _State) ->
  true = inet_db:register_socket(Sock, inet_tcp),
  ?DEBUG("Incoming client LSock:~p, Sock:~p", [LSock, Sock]),
  %% TODO 启动一个新的玩家进程, 并将tcp控制权交给此进程
  accept(_State);
handle_info({inet_async, LSock, Ref, {error, closed}}, State) ->
  ?INFO("Socket closed, LSock:~p, Ref:~p", [LSock, Ref]),
  {stop, normal, State};
handle_info({inet_async, LSock, Ref, Error}, State) ->
  ?ERROR("Accept error, LSock:~p, Ref:~p, Error:~p", [LSock, Ref, Error]),
  accept(State);
handle_info({'DOWN', _Ref, process, Pid, Info}, State = #state{count = Count}) ->
  ?INFO("Client process ~p closed connection, reason:~p", [Pid, Info]),
  {noreply, State#state{count = Count - 1}};
handle_info(_Info, State) ->
  {noreply, State}.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #state{}) -> term()).
terminate(_Reason, #state{socket = Socket}) ->
  ?INFO("network app receive terminate for reason ~p", [_Reason]),
  case erlang:is_port(Socket) of
    true -> close_socket(Socket);
    false -> ok
  end,
  ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
                  Extra :: term()) ->
                   {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc start TCP socket listen.
start_listen() ->
  ?DEBUG("network start_listen/0 called~n"),
  Port = ?DEFAULT_PORT, % TODO replace with the port from config
  case gen_tcp:listen(Port, ?TCP_OPTIONS) of
    {ok, Socket} ->
      ?INFO("Network app started at port ~p", [Port]),
      Socket;
    {error, Reason} ->
      ?ERROR("Error while open socket on port ~p, reason:~p", [Port, Reason]),
      exit(Reason)
  end.

%% @doc start to accept client connection.
accept(State = #state{socket = Socket, count = Count}) ->
  ?INFO("network app ready for accepting connections"),
  case prim_inet:async_accept(Socket, -1) of
    {ok, Ref} -> {noreply, State#state{count = Count + 1, ref = Ref}};
    Error ->
      ?ERROR("Error while accept client connection, reason:~p~n", [Error]),
      {stop, {cannot_accept, Error}, State}
  end.

%% @doc close the socket.
close_socket(Port) ->
  unlink(Port),
  catch erlang:port_close(Port),
  receive {tcp_closed, Port} -> ok after 0 -> ok end.