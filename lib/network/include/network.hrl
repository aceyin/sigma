%%%-------------------------------------------------------------------
%%% @author ace
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 3月 2020 11:32 下午
%%%-------------------------------------------------------------------
-author("ace").
-ifndef(NETWORK_HRL).
-define(NETWORK_HRL, true).

-include("logger.hrl").
%% @doc
%% Socket options.
%% @see inet:setopts/2
%% @end
-define(DEFAULT_OPTIONS, [
  binary,
  % If the value is true, which is the default, everything received from the socket is
  % sent as messages to the receiving process.
  % If the value is false (passive mode), the process must explicitly receive incoming data
  % by calling gen_tcp:recv/2,3, gen_udp:recv/2,3, or gen_sctp:recv/1,2 (depending on the type of socket).
  {active, false},
  % Normally, when an Erlang process sends to a socket, the driver tries to send the data immediately.
  % If that fails, the driver uses any means available to queue up the message to be sent
  % whenever the operating system says it can handle it.
  % Setting {delay_send, true} makes all messages queue up.
  % The messages sent to the network are then larger but fewer.
  % The option affects the scheduling of send requests versus Erlang processes instead of
  % changing any real property of the socket.
  % The option is implementation-specific.
  {delay_send, true},
  % If Boolean == true, option TCP_NODELAY is turned on for the socket,
  % which means that also small amounts of data are sent immediately.
  {nodelay, true},
  % Enables/disables periodic transmission on a connected socket when no other data is exchanged.
  % If the other end does not respond, the connection is considered broken and an error message
  % is sent to the controlling process.
  {keepalive, true},
  % This option is set to true by default.
  % The only reason to set it to false is
  % if you want to continue sending data to the socket after a close is detected
  {exit_on_close, false},
  {send_timeout, 4000},
  % Allows or disallows local reuse of port numbers.
  % By default, reuse is disallowed.
  {reuseaddr, true}
]).

%% @doc
%% 网络配置参数
%% @end
-record(net_config, {
  %% TCP 选项
  options = ?DEFAULT_OPTIONS :: list(),
  %% 处理网络数据的接收器模块名称 #{sup=>module(), mod=>module()}
  receiver :: map(),
  %% 监听端口
  port :: non_neg_integer(),
  %% 最大连接数
  max_conn :: non_neg_integer()
}).

%% @doc
%% 网络进程的状态记录
%% @end
-record(net_state, {
  % the network receiver of this network app
  receiver :: map(),
  % the server socket
  server_socket = 0 :: port(),
  % current used socket options.
  options = ?DEFAULT_OPTIONS :: list(),
  % client count
  count = 0 :: non_neg_integer(),
  % max allowed connections
  max = 1000 :: non_neg_integer(),
  % prim_inet:async_accept 返回的一个引用参数
  % 可用在新的网络连接建立时(handle_info回调函数)做模式匹配用
  ref :: reference()
}).

-endif.