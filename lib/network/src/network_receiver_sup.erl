%%%-------------------------------------------------------------------
%%% @author ace
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 4月 2020 12:02 下午
%%%-------------------------------------------------------------------
-module(network_receiver_sup).
-author("ace").

-behaviour(supervisor).
-include("logger.hrl").
%% API
-export([start_link/1]).
%% Supervisor callbacks
-export([init/1]).

start_link([Mod, Fun]) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [Mod, Fun]).

%% @doc
%% 所有 socket 接管进程的 supervisor 模块.
%% 每当有新的客户端连接过来时, 会使用 sigma.config 中 receiver 选项
%% 配置的模块作为该连接的处理进程. network_server 会为每个新的 socket
%% 创建一个单独的进程来负责该 socket 的处理, 本 supervisor 就是负责创建
%% network_receiver 进程的.
%% @end
init([Mod, Fun]) ->
  ChildSpec = {
    % network_receiver 实例的模块名,
    % 亦即 network 配置参数的 receiver 中配置的 mod
    Mod,
    % network_receiver 实例的启动规范,
    % 因为每个 receiver 都 mixin 了 receiver_base 的通用部分,为了在 receiver_base 中能
    % 调用到具体的 receiver 的callback函数, 所以在 args 里面需要将 Mod 再传递进去一次.
    {Mod, Fun, [Mod]},
    temporary,
    2000000,
    worker,
    [echo]
  },
  {ok, {{simple_one_for_one, 10, 10}, [ChildSpec]}}.