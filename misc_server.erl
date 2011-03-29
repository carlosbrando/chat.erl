-module (misc_server).
-behaviour (gen_server).
-export ([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export ([start_link/0, add/2, subtract/2, add_and_subtract/2, stop/0]).

% Client functions
start_link () ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add (A, B) ->
  gen_server:call(?MODULE, {add, A, B}).

subtract (A, B) ->
  gen_server:call(?MODULE, {subtract, A, B}).

add_and_subtract(A, B) ->
  [misc_server:add(A, B), misc_server:subtract(A, B)].

stop () ->
  gen_server:cast(?MODULE, stop).

init ([]) ->
  {ok, []}.

handle_call ({subtract, A, B}, _From, State) ->
  {reply, {ok, A - B}, State};
handle_call ({add, A, B}, _From, State) ->
  {reply, {ok, A + B}, State};
handle_call (_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast (stop, State) ->
  {stop, normal, State};
handle_cast (_Msg, State) ->
  {noreply, State}.

handle_info (_Info, State) ->
  io:format("Info message received: ~p~n", [_Info]),
  {noreply, State}.

terminate (_Reason, _State) ->
  io:format("Server is stopping...~n"),
  ok.

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.

