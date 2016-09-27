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
        io:format("~nBody:~n~p~n", [Body]),
        {Qs, Req4} = cowboy_req:qs(Req3),
        io:format("~nQs:~n~p~n", [Qs]),
        {ok, Req5} =
          cowboy_req:reply(
            200,
            [{<<"content-type">>, <<"text/plain">>}],
            Body,
            Req4),
        Req5;
      _Other ->
        {ok, Req3} = cowboy_req:reply(200, [], <<"Other request">>, Req2),
        Req3
    end,
  {ok, NewReq, State}.

terminate(_Reason, _Req, _State) ->
	ok.
