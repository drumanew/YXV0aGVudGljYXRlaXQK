-module(seq_calc_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(MaxWorkers) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, MaxWorkers, []).

init(_MaxWorkers) ->
	Procs = [],
	{ok, {{one_for_one, 1, 5}, Procs}}.
