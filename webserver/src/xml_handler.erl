-module(xml_handler).
-behaviour(gen_fsm).

-define (DEBUG, ok).

-ifdef (DEBUG).
-define (DBG (F, A),
         io:format (standard_error,
                    "* ~s:~w: " ++ F ++ "~n",
                    [?MODULE, ?LINE | A])).
-else.
-define (DBG (F, A), ok).
-endif.

%% API.
-export([new_xml/1]).

%% gen_fsm.
-export([init/1]).
-export([handle_event/3]).
-export([handle_sync_event/4]).
-export([handle_info/3]).
-export([terminate/3]).
-export([code_change/4]).

-export([initial/2]).
-export([initial/3]).
-export([parsing/2]).
-export([parsing/3]).


-record(state, { xml }).

%% API.

new_xml(Xml) ->
  gen_fsm:start_link(?MODULE, Xml, []).

%% gen_fsm.

init(Xml) ->
  Pid = self(),
  ?DBG("init: ~p", [Pid]),
  erlsom:parse_sax(
    Xml,
    [],
    fun
      (Event = endDocument, Acc) ->
        gen_fsm:send_all_state_event(Pid, Event),
        Acc;
      (Event, Acc) ->
        gen_fsm:send_event(Pid, Event),
        Acc
    end),
  {ok, initial, #state{ xml = Xml }}.

initial(startDocument, StateData) ->
  {next_state, parsing, StateData};
initial(_Event, StateData) ->
  {next_state, initial, StateData}.

parsing(_Event, StateData) ->
  ?DBG("~p", [_Event]),
  case Event of
    {}
  {next_state, parsing, StateData}.

handle_event(endDocument, _StateName, StateData) ->
  {stop, normal, StateData};
handle_event(_Event, StateName, StateData) ->
  {next_state, StateName, StateData}.

initial(_Event, _From, StateData) ->
  {reply, ignored, initial, StateData}.

parsing(_Event, _From, StateData) ->
  {reply, ignored, parsing, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
  {reply, ignored, StateName, StateData}.

handle_info(_Info, StateName, StateData) ->
  {next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
  ?DBG("terminate: ~p", [self()]),
  ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

%% Private

