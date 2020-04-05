%%%-------------------------------------------------------------------
%%% @author ace
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 用来分派网络请求的调度模块基类.
%%% @end
%%% Created : 04. 4月 2020 10:36 下午
%%%-------------------------------------------------------------------
-module(network_dispatcher).
-author("ace").

%% API
-export([]).


%% 接受信息
async_recv(Sock, Length, Timeout) when is_port(Sock) ->
  case prim_inet:async_recv(Sock, Length, Timeout) of
    {error, Reason} -> 	throw({Reason});
    {ok, Res}       ->  Res;
    Res             ->	Res
  end;
async_recv(_Sock,_Len,_Timeout)->
  {error, "param[1] is not a port"}.