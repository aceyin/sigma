network
=====
配置项
```erlang
{ network, #{
  %% TCP 连接选项
  options => [
    %% @doc 设置是否允许重用端口号, 默认: true @end
    % {reuseaddr, true},
    %% @doc 设置是否将消息缓存起来之后再一次性发送(可减少网络发送次数). 默认: true @end
    % {delay_send, true},
    %% @doc 设置系统的 TCP_NODELAY 属性, 当设为 true 时意味着无论多小的包, 都会立即发送. 默认: true @end
    % {nodelay, true},
    %% @doc 设置OTP网络模块在将数据包发往底层的 TCP 栈之前的最大等待时间, 超过这个时间的send操作将会返回{error, timeout}.  @end
    {send_timeout, 8000}
  ],
  %% @doc 本系统的网络监听端口 @end
  port => 8971,
  %% @doc 本系统的最大同时在线连接数 @end
  max_conn => 1000,
  %% @doc 设置处理网络数据的模块和其supervisor模块名. @end
  receiver => #{sup => echo_sup, mod => echo}
}}.
```