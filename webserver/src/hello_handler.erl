-module(hello_handler).

-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {}).

init(_, Req, _Opts) ->
  {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
  {Method, Req2} = cowboy_req:method(Req),
  NewReq =
    case Method of
      <<"POST">> ->
        {ok, Body, Req3} = cowboy_req:body(Req2),
        xml_handler:new_xml(Body),
        Req3;
      _Other ->
        Req2
    end,
  {ok, NewReq1} = cowboy_req:reply(200, NewReq),
  {ok, NewReq1, State}.

terminate(_Reason, _Req, _State) ->
  ok.
