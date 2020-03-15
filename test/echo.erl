%%%-------------------------------------------------------------------
%%% @author ace
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 3月 2020 10:12 下午
%%%-------------------------------------------------------------------
-module(echo).
-author("ace").

%% API
-export([go/0, loop/0]).

go() ->
  Pid = spawn(echo, loop, []),
  io:format("send {hello, hello} to ~p from ~p~n", [Pid, self()]),
  Pid ! {hello, hello},
  io:format("send {From, hello} to ~p from ~p~n", [Pid, self()]),
  Pid ! {self(), hello},
  receive
    {Pid, Msg} ->
      io:format("~p received ~w from ~p~n", [self(), Msg, Pid])
  end,

  Pid ! {self(), halo},
  Pid ! stop.

loop() ->
  receive
    {Msg1, Msg2} when is_atom(Msg1)->
      io:format("~p received ~w ~w~n", [self(), Msg1, Msg2]);
    {From, Msg} when is_pid(From) ->
      io:format("~p received ~w from ~p~n", [self(), Msg, From]),
      From ! {self(), Msg},
      loop();
    {From, stop} when is_pid(From) ->
      io:format("~p received ~w from ~p~n", [self(), stop, From]),
      ok;
    stop ->
      io:format("~p received ~w~n", [self(), stop]),
      ok
  end.
