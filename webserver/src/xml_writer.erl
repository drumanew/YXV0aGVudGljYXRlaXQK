-module(xml_writer).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([start_link/1]).
-export([write/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, { file }).
-define(DEFAULT_FILENAME, "/tmp/web_requests_data.csv").

%% API.
-spec start_link() -> {ok, pid()}.
start_link() ->
  start_link(?DEFAULT_FILENAME).

-spec start_link(Filename :: string()) -> {ok, pid()}.
start_link(Filename) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Filename, []).

-spec write(Row :: list()) -> ok.
write(Row) ->
  gen_server:cast(?MODULE, {write, Row}).

%% gen_server.

init(Filename) ->
  {ok, #state{ file = Filename }}.

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast({write, Row}, State = #state{ file = Filename }) ->
  do_write(Filename, Row),
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% private
do_write (Filename, Row) ->
  {ok, File} = file:open(Filename, [append]),
  csv_gen:row(File, Row),
  file:close(File).
