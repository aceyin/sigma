%%%-------------------------------------------------------------------
%%% @author ace
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 基于 gen_server 封装的一套专门用来处理网络连接的 behaviour 封装.
%%% @end
%%% Created : 18. 4月 2020 2:57 下午
%%%-------------------------------------------------------------------
-module(network_receiver).
-author("ace").

-behaviour(gen_server).
-include("logger.hrl").

%% API
-export([start_link/0, take_over/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%%% callbacks

%% @doc
%% 该函数会在 gen_server 的 init/1 回调函数中被调用,
%% 用来取代 init/1 函数来进行模块的初始化工作.
%% Args 参数即为 init/1 函数所接收到的参数.
%% 该函数的返回值, 将作为 init/1 的返回值被 gen_server 使用.
%% @end
-callback(ready(Args :: term()) ->
  {ok, State :: term()} | {ok, State :: term(), timeout()} |
  {stop, Reason :: term()} | ignore
).

%% @doc
%% 在本服务收到所托管的Socket的数据时会被自动调用.
%% @end
-callback(on_receive(Data) -> ok).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% 启动本服务进程.
%% 该函数会被 network_receiver_sup 这个 supervisor 调用.
%% @end
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc
%% 接管刚刚新建立的客户端连接.
%% 该函数会在 network_server 中被调用.
%% 调用之前必须有一个新的用来处理后续网络数据的进程已经启动了.
%% @param CSock::port() 代表客户端连接的 socket 引用.
%% @param Pid::pid() 即将接管 CSock 的进程ID.
%% @end
-spec(take_over(Pid :: pid(), CSock :: port()) -> ok).
take_over(Pid, CSock) ->
  gen_server:cast(Pid, {active, CSock}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @doc Initializes the server @end
init(Args) ->
  process_flag(trap_exit, true),
  case erlang:function_exported(?MODULE, ready, 1) of
    true -> ?MODULE:ready(Args);
    _ -> {ok, #state{}}
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #state{}) ->
                   {reply, Reply :: term(), NewState :: #state{}} |
                   {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
                   {noreply, NewState :: #state{}} |
                   {noreply, NewState :: #state{}, timeout() | hibernate} |
                   {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
                   {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
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
  ?MODULE:on_receive(Data),
  async_recv(CSock, 0, -1),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

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
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
                  Extra :: term()) ->
                   {ok, NewState :: #state{}} | {error, Reason :: term()}).
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