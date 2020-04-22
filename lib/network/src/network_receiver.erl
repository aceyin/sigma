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
-callback(on_receive(Data :: term()) -> ok).

-optional_callbacks([ready/1, on_receive/1]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% 启动本服务进程.
%% 该函数会被 network_receiver_sup 这个 supervisor 调用.
%% @end
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


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
