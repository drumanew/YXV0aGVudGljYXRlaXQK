-module(seq_calc_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-define (DEFAULT_NUMBER_OF_WORKERS, 500).

start(_Type, _Args) ->
  case application:get_env(seq_calc, maximum_number_of_workers) of
    {ok, N} -> seq_calc_sup:start_link(N);
    _       -> seq_calc_sup:start_link(?DEFAULT_NUMBER_OF_WORKERS)
  end.


stop(_State) ->
	ok.
