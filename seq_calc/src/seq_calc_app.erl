-module(seq_calc_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-define(DEFAULT_NUMBER_OF_WORKERS, 500).
-define(DEFAULT_MIN_NUM, 1).
-define(DEFAULT_MAX_NUM, 999999).
-define(DEFAULT_STORE_VALUES, false).

start(_Type, _Args) ->
  MaxWorkers  = get_arg(max_workers, ?DEFAULT_NUMBER_OF_WORKERS),
  MinNum      = get_arg(min_num, ?DEFAULT_MIN_NUM),
  MaxNum      = get_arg(max_num, ?DEFAULT_MAX_NUM),
  StoreValues = get_arg(store_values, ?DEFAULT_STORE_VALUES),
  seq_calc_sup:start_link(MaxWorkers, MinNum, MaxNum, StoreValues).

stop(_State) ->
	ok.


get_arg (Arg, Default) ->
  case application:get_env(seq_calc, Arg) of
    {ok, V} -> V;
    _       -> Default
  end.
