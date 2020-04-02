%%%-------------------------------------------------------------------
%%% @author ace
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. 3月 2020 11:39 下午
%%%-------------------------------------------------------------------
-author("ace").
-define(DEFAULT_LOGGER, #{
  level => debug,
  format => [time, " ", file, ":", line, " ", level, ": ", msg, "\n"],
  file => "server.log"
}).