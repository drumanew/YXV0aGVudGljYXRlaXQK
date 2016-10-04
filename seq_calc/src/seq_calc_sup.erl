-module(seq_calc_sup).

-behaviour(supervisor).

-export([start_link/4]).
-export([init/1]).

-define(range_length(Min, Max), (Max - Min + 1)).

start_link(MaxWorkers, MinNum, MaxNum, StoreValues) ->
	supervisor:start_link({local, ?MODULE},
                        ?MODULE,
                        {MaxWorkers, MinNum, MaxNum, StoreValues}).

init({MaxWorkers, MinNum, MaxNum, StoreValues}) ->
  Collector = #{id    => seq_calc_collecter,
                start => {seq_calc_collecter, start_link, [StoreValues]}},
  Tasks   = create_tasks({MinNum, MaxNum}, MaxWorkers),
	Workers =
    [#{id      => {seq_calc_worker, Task},
       start   => {seq_calc_worker, start_link, [Task]},
       restart => transient}
     || Task <- Tasks],
  Procs = [Collector | Workers],
	{ok, {{one_for_one, 1, 5}, Procs}}.

create_tasks (ValuesRange = {MinNum, MaxNum}, MaxWorkers) ->
  ValuesCount     = ?range_length(MinNum, MaxNum),
  ValuesPerWorker = ValuesCount div MaxWorkers,
  RemCount        = ValuesCount rem MaxWorkers,
  create_tasks(ValuesRange, ValuesPerWorker, RemCount, []).


create_tasks (ValuesRange, ValuesPerWorker, 0, Tasks) ->
  create_tasks(ValuesRange, ValuesPerWorker, Tasks);
create_tasks ({Min, Max}, ValuesPerWorker, RemCount, Tasks) ->
  create_tasks({Min + ValuesPerWorker + 1, Max},
               ValuesPerWorker,
               RemCount - 1,
               [{Min, Min + ValuesPerWorker} | Tasks]).


create_tasks (_ValuesRange = {Min, Max}, _ValuesPerWorker, Tasks)
  when Min > Max ->
  Tasks;
create_tasks (ValuesRange = {Min, Max}, ValuesPerWorker, Tasks)
  when ValuesPerWorker > ?range_length(Min, Max) ->
  create_tasks({Max + 1, Max}, ValuesPerWorker, [ValuesRange | Tasks]);
create_tasks (_ValuesRange = {Min, Max}, ValuesPerWorker, Tasks) ->
  create_tasks({Min + ValuesPerWorker, Max},
               ValuesPerWorker,
               [{Min, Min + ValuesPerWorker - 1} | Tasks]).


