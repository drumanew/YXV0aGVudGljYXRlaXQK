-module(webserver_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  case application:get_env(webserver, file) of
    {ok, Filename} -> webserver_sup:start_link(Filename);
    _              -> webserver_sup:start_link()
  end.

stop(_State) ->
  ok.
