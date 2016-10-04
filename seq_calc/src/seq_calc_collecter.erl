-module(seq_calc_collecter).
-behaviour(gen_server).

%% API.
-export([start_link/1]).
-export([notify/2]).
-export([get_current/0]).
-export([get/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, { value, length, store, started, last_update }).

%% API.

-spec start_link(StoreValues :: boolean()) -> {ok, pid()}.
start_link(StoreValues) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, StoreValues, []).

notify (Value, Length) ->
  gen_server:cast(?MODULE, {ntf, Value, Length}).

get_current () ->
  gen_server:call(?MODULE, {get, current}, infinity).

get (Value) ->
  gen_server:call(?MODULE, {get, Value}, infinity).

%% gen_server.

init(false) ->
  {ok, #state{ store = false, started = erlang:timestamp() }};
init(true) ->
  ets:new(?MODULE, [ordered_set, protected, named_table]),
	{ok, #state{ store = true, started = erlang:timestamp() }}.

handle_call({get, current}, _From, State = #state{ value       = Value,
                                                   length      = Length,
                                                   started     = Started,
                                                   last_update = Updated }) ->
  io:format("Got this value in ~f ms~n",
            [timer:now_diff(Updated, Started) / 1000]),
  {reply, {Value, Length}, State};
handle_call({get, Value}, _From, State = #state{ store   = true,
                                                 started = Started }) ->
  Length =
    case ets:lookup(?MODULE, Value) of
      [{Value, {L, Updated}}] ->
        io:format("Got this value in ~f ms~n",
                  [timer:now_diff(Updated, Started) / 1000]),
        L;
      _ -> undefined
    end,
  {reply, {Value, Length}, State};
handle_call({get, _}, _From, State) ->
  {reply, {error, not_store}, State};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast({ntf, Value, Length}, State = #state{ value  = undefined,
                                                  length = undefined,
                                                  store  = StoreValues }) ->
  if
    StoreValues -> ets:insert(?MODULE, {Value, {Length, erlang:timestamp()}});
    true -> ok
  end,
  {noreply, State#state{ value       = Value,
                         length      = Length,
                         last_update = erlang:timestamp() }};
handle_cast({ntf, Value, Length}, State = #state{ length = CurrLength,
                                                  store  = StoreValues })
  when Length > CurrLength ->
  if
    StoreValues -> ets:insert(?MODULE, {Value, {Length, erlang:timestamp()}});
    true -> ok
  end,
  {noreply, State#state{ value       = Value,
                         length      = Length,
                         last_update = erlang:timestamp() }};
handle_cast({ntf, Value, Length}, State = #state{ store = StoreValues }) ->
  if
    StoreValues -> ets:insert(?MODULE, {Value, {Length, erlang:timestamp()}});
    true -> ok
  end,
  {noreply, State};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

