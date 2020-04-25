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
-include("logger.hrl").

%%% callbacks

%% @doc
%% 在本服务收到所托管的Socket的数据时会被自动调用.
%% @end
-callback(on_receive(Data :: term()) -> ok).
-callback(ready(Args :: term()) -> {ok, State :: term()}).
-optional_callbacks([ready/1]).
