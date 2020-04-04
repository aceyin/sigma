%%%-------------------------------------------------------------------
%%% @author ace
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. 3月 2020 11:39 下午
%%%-------------------------------------------------------------------
-author("ace").
-ifndef(LOGGER_HRL).
-define(LOGGER_HRL, true).
-define(DEFAULT_LOGGER, #{
  level => debug,
  format => [time, " ", file, ":", line, " ", level, ": ", msg, "\n"],
  file => "server.log"
}).

-define(ERROR(A), ?DO_LOG(error, [A])).
-define(ERROR(A, B), ?DO_LOG(error, [A, B])).
-define(ERROR(A, B, C), ?DO_LOG(error, [A, B, C])).

-define(WARNING(A), ?DO_LOG(warning, [A])).
-define(WARNING(A, B), ?DO_LOG(warning, [A, B])).
-define(WARNING(A, B, C), ?DO_LOG(warning, [A, B, C])).

-define(INFO(A), ?DO_LOG(info, [A])).
-define(INFO(A, B), ?DO_LOG(info, [A, B])).
-define(INFO(A, B, C), ?DO_LOG(info, [A, B, C])).

-define(DEBUG(A), ?DO_LOG(debug, [A])).
-define(DEBUG(A, B), ?DO_LOG(debug, [A, B])).
-define(DEBUG(A, B, C), ?DO_LOG(debug, [A, B, C])).

-define(LOCATION, #{
  mfa=>{?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
  line=>?LINE,
  file=>?FILE,
  filename=>filename:basename(?FILE)
}).

%%%-----------------------------------------------------------------
%%% Internal, i.e. not intended for direct use in code - use above
%%% macros instead!
-define(DO_LOG(Level, Args),
  case logger:allow(Level, ?MODULE) of
    true ->
      apply(logger, macro_log, [?LOCATION, Level | Args]);
    false ->
      ok
  end).

-endif.