-module(seq_calc_worker).
-behaviour(gen_server).

%% API.
-export([start_link/1]).
-export([get_state/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, { task }).

%% API.

-spec start_link(ValuesRange :: {integer(), integer()}) -> {ok, pid()}.
start_link(ValuesRange) ->
	gen_server:start_link(?MODULE, ValuesRange, []).

get_state(Pid) ->
  gen_server:call(Pid, state).

%% gen_server.

init(ValuesRange) ->
	{ok, #state{ task = ValuesRange }, 0}.

handle_call(state, _From, State = #state{ task = Task}) ->
  {reply, Task, State};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(timeout, State = #state{ task = Task }) ->
  NewTask = work(Task),
  {noreply, State#state{ task = NewTask }};
handle_info(done, State) ->
  {stop, normal, State};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% private

work({Value, Max}) when Value == Max + 1 ->
  self() ! done,
  {Value, Max};
work({Value, Max}) ->
  notify(Value, get_chain_length(Value)),
  self() ! timeout,
  {Value + 1, Max}.

get_chain_length (Value) ->
  get_chain_length(Value, 1).

get_chain_length (1, Length) ->
  Length;
get_chain_length (Value, Length) when Value rem 2 == 0 ->
  get_chain_length (Value div 2, Length + 1);
get_chain_length (Value, Length) ->
  get_chain_length (Value*3 + 1, Length + 1).

notify(Value, Length) ->
  seq_calc_collecter:notify(Value, Length).
