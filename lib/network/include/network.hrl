%%%-------------------------------------------------------------------
%%% @author ace
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 3月 2020 11:32 下午
%%%-------------------------------------------------------------------
-author("ace").

%% Socket options.
%% @see inet:setopts/2
-record(socket_option, {
  % mode: binary | list |
  mode = binary :: atom(),
  % If the value is true, which is the default, everything received from the socket is
  % sent as messages to the receiving process.
  % If the value is false (passive mode), the process must explicitly receive incoming data
  % by calling gen_tcp:recv/2,3, gen_udp:recv/2,3, or gen_sctp:recv/1,2 (depending on the type of socket).
  active = true :: boolean(),
  % The size of the user-level buffer used by the driver.
  % Not to be confused with options sndbuf and recbuf, which correspond to the Kernel socket buffers.
  % For TCP it is recommended to have val(buffer) >= val(recbuf) to avoid performance issues
  % because of unnecessary copying.
  % Note that this is also the maximum amount of data that can be received from a single recv call.
  % If you are using higher than normal MTU consider setting buffer higher.
  buffer :: number(),
  % The minimum size of the send buffer to use for the socket.
  % You are encouraged to use getopts/2, to retrieve the size set by your operating system.
  sndbuf :: number(),
  % Normally, when an Erlang process sends to a socket, the driver tries to send the data immediately.
  % If that fails, the driver uses any means available to queue up the message to be sent
  % whenever the operating system says it can handle it.
  % Setting {delay_send, true} makes all messages queue up.
  % The messages sent to the network are then larger but fewer.
  % The option affects the scheduling of send requests versus Erlang processes instead of
  % changing any real property of the socket.
  % The option is implementation-specific.
  delay_send = false :: boolean(),
  %% If Boolean == true, option TCP_NODELAY is turned on for the socket,
  %% which means that also small amounts of data are sent immediately.
  nodelay = true :: boolean(),
  % This option is only meaningful if option binary was specified when the socket was created.
  % If option header is specified, the first Size number bytes of data received from the socket
  % are elements of a list, and the remaining data is a binary specified as the tail of the same list.
  % For example, if Size == 2, the data received matches [Byte1,Byte2|Binary].
  header = 4 :: number(),
  keepalive = true :: boolean(),
  % Defines the type of packets to use for a socket. Possible values:
  % * raw | 0   : No packaging is done.
  % * 1 | 2 | 4 : Packets consist of a header specifying the number of bytes in the packet,
  %               followed by that number of bytes. The header length can be one, two, or four bytes,
  %               and containing an unsigned integer in big-endian byte order.
  %               Each send operation generates the header, and the header is stripped off on
  %               each receive operation.
  %               The 4-byte header is limited to 2Gb.
  % * others    : Which will not be used in a game server. Possible values are:
  %               asn1 | cdr | sunrm | fcgi | tpkt | line | http | http_bin | httph | httph_bin
  packet = 4 :: number(),
  % Sets the maximum allowed length of the packet body.
  % If the packet header indicates that the length of the packet is longer than the maximum allowed length,
  % the packet is considered invalid. The same occurs if the packet header is too large for
  % the socket receive buffer.
  packet_size :: number(),
  exit_on_close = false :: boolean(),
  send_timeout = 4000 :: non_neg_integer(),
  reuseaddr = true :: boolean()
}).

%% Network config.
-record(network, {
  ip = {0, 0, 0, 0} :: tuple(),
  port :: non_neg_integer(),
  options :: #socket_option{}
}).