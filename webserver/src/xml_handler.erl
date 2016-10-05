-module(xml_handler).
-behaviour(gen_fsm).

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
-export([attributes/2]).
-export([attributes/3]).

-record(record, { gtin, name, desc, company }).
-record(st, { pending = #record{},
              records = [] }).

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
  {ok, initial, #st{}}.

initial(startDocument, StateData) ->
  {next_state, parsing, StateData};
initial(_Event, StateData) ->
  {next_state, initial, StateData}.

parsing({startElement, _, "BaseAttributeValues", _, _}, StateData) ->
  {next_state, attributes, StateData};
parsing(_Event, StateData) ->
  {next_state, parsing, StateData}.

attributes({endElement, _, "BaseAttributeValues", _},
           StateData = #st{ pending = #record{ gtin = GTIN, name = Name } })
  when GTIN == undefined orelse Name == undefined ->
  {next_state, parsing, StateData#st{ pending = #record{} }};
attributes({endElement, _, "BaseAttributeValues", _},
           StateData = #st{ pending = R, records = Recs }) ->
  {next_state, parsing, StateData#st{ pending = #record{},
                                      records = [R | Recs] }};
attributes({startElement, _,"value", _, Attrs},
           StateData = #st{ pending = R }) ->
  case lists:keyfind("baseAttrId", 2, Attrs) of
    {attribute, "baseAttrId", _, _, AttrId} ->
      case lists:keyfind("value", 2, Attrs) of
        {attribute, "value", _, _, AttrVal} ->
          UpdR = set_attr(AttrId, AttrVal, R),
          {next_state, attributes, StateData#st{ pending = UpdR }};
        _ -> {next_state, attributes, StateData}
      end;
    _ -> {next_state, attributes, StateData}
  end;
attributes(_Event, StateData) ->
  {next_state, attributes, StateData}.

handle_event(endDocument, _StateName, StateData) ->
  {stop, normal, StateData};
handle_event(_Event, StateName, StateData) ->
  {next_state, StateName, StateData}.

initial(_Event, _From, StateData) ->
  {reply, ignored, initial, StateData}.

parsing(_Event, _From, StateData) ->
  {reply, ignored, parsing, StateData}.

attributes(_Event, _From, StateData) ->
  {reply, ignored, attributes, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
  {reply, ignored, StateName, StateData}.

handle_info(_Info, StateName, StateData) ->
  {next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData = #st{ records = Recs }) ->
  save_records(Recs),
  ?DBG("terminate: ~p", [self()]),
  ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

%% Private

set_attr(_AttrId = "PROD_COVER_GTIN", AttrVal, Record) ->
  ?DBG("set attribute ~s", [_AttrId]),
  Record#record{ gtin = AttrVal };
set_attr(_AttrId = "PROD_NAME", AttrVal, Record) ->
  ?DBG("set attribute ~s", [_AttrId]),
  Record#record{ name = AttrVal };
set_attr(_AttrId = "PROD_DESC", AttrVal, Record) ->
  ?DBG("set attribute ~s", [_AttrId]),
  Record#record{ desc = AttrVal };
set_attr(_AttrId = "BRAND_OWNER_NAME", AttrVal, Record) ->
  ?DBG("set attribute ~s", [_AttrId]),
  Record#record{ company = AttrVal };
set_attr(_, _, Record) ->
  Record.


save_records (Recs) ->
  [ write(Rec) || Rec <- Recs ].

write (#record{ gtin    = GTIN,
                name    = Name,
                desc    = Desc,
                company = Company }) ->
  Row = lists:map(fun replace_undefined/1, [GTIN, Name, Desc, Company]),
  xml_writer:write(Row).

replace_undefined (undefined) -> "";
replace_undefined (_Other)    -> _Other.
