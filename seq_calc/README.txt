Make application with:

  $ make

Run application with:

  $ erl -pa ebin -eval "application:start(seq_calc)" [-seq_calc PARAMS...]

Parameters:
  max_workers N :: integer()     : maximum number of workers, default is 500
  store_values Store :: bool()   : save lengths of all calculated values, default is 'false'
  min_num Min :: integer()       :
  max_num Max :: integer()       : override test on range Min-Max, default is 1-999999

Get currently calculated maximum chain length and related number:

  1> seq_calc_collecter:get_current().
  Got this value in 275.348000 ms
  {837799,525}
  2>

  837799 - starting number  produces longest chain
  525    - chain length

