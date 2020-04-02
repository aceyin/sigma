%%%-------------------------------------------------------------------
%%% @author ace
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. 4月 2020 11:11 下午
%%%-------------------------------------------------------------------
-module(logger_conf).
-author("ace").

%% API
-export([]).

conf() ->
  #{
    handlers => [
      #{
        config => #{
          handler_pid => 1,
          mode_tab => 1,
          type => standard_io
        },
        filter_default => stop,
        filters => [
          {remote_gl, {fun logger_filters:remote_gl/2, stop}},
          {domain, {fun logger_filters:domain/2, {log, super, [otp, sasl]}}},
          {no_domain, {fun logger_filters:domain/2, {log, undefined, []}}}
        ],
        formatter => {
          logger_formatter, #{legacy_header => true, single_line => false}
        },
        id => default,
        level => debug,
        module => logger_std_h
      }
    ],
    module_levels => [],
    primary => #{
      filter_default => log,
      filters => [],
      level => notice
    }
  }.