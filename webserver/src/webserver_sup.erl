-module(webserver_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([start_http/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Procs = [#{id      => xml_writer,
             start   => {xml_writer, start_link, []},
             restart => transient},
           #{id      => start_http,
             start   => {?MODULE, start_http, []}}],
  {ok, {{one_for_one, 1, 5}, Procs}}.

start_http () ->
  Dispatch = cowboy_router:compile([
      {'_', [{"/", hello_handler, []}]}
  ]),
  cowboy:start_http(my_http_listener, 100, [{port, 8080}],
      [{env, [{dispatch, Dispatch}]}]
  ),
  ignore.
