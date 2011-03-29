%%%-------------------------------------------------------------------
%%% File:      message_router.erl
%%% @author    Carlos Brando <eduardobrando@gmail.com> [http://www.nomedojogo.com]
%%% @copyright 2011 Carlos Brando
%%% @doc  
%%%
%%% @end  
%%%
%%% @since 2011-03-29 by Carlos Brando
%%%-------------------------------------------------------------------
-module(message_router).
-author('eduardobrando@gmail.com').

-behaviour(gen_server).

%% API
-export([start_link/0, send_chat_message/2, register_nick/2, unregister_nick/1, shutdown/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define (SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end 
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

send_chat_message (ClientName, MessageBody) ->
  gen_server:call({global, ?SERVER}, {send_chat_msg, ClientName, MessageBody}).

register_nick (ClientName, ClientPid) ->
  gen_server:call({global, ?SERVER}, {register_nick, ClientName, ClientPid}).

unregister_nick (ClientName) ->
  gen_server:call({global, ?SERVER}, {unregister_nick, ClientName}).

shutdown () ->
  gen_server:cast(?SERVER, stop).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% @doc Initiates the server
%% @end 
%%--------------------------------------------------------------------
init([]) ->
  message_store:start_link(),
  {ok, dict:new()}.

%%--------------------------------------------------------------------
%% @spec 
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Handling call messages
%% @end 
%%--------------------------------------------------------------------
handle_call ({send_chat_msg, ClientName, MessageBody}, _From, Clients) ->
  case dict:find(ClientName, Clients) of
    {ok, ClientPid} ->
      ClientPid ! {printmsg, MessageBody};
    error ->
      message_store:save_message(ClientName, MessageBody),
      io:format("Archived message for ~p~n", [ClientName])
  end,
  {reply, ok, Clients};
  
handle_call ({register_nick, ClientName, ClientPid}, _From, Clients) ->
  Messages = message_store:find_messages(ClientName),
  lists:foreach(fun(Msg) -> ClientPid ! {printmsg, Msg} end, Messages),
  {reply, ok, dict:store(ClientName, ClientPid, Clients)};

handle_call ({unregister_nick, ClientName}, _From, Clients) ->
  UpdatedClients = case dict:find(ClientName, Clients) of
    {ok, ClientPid} ->
      ClientPid ! stop,
      dict:erase(ClientName, Clients);
    error ->
      io:format("Error! Unknown client: ~p~n", [ClientName]),
      Clients
  end,
  {reply, ok, UpdatedClients};

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% @doc Handling cast messages
%% @end 
%%--------------------------------------------------------------------
handle_cast (stop, State) ->
  message_store:shutdown(),
  {stop, normal, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
%% @end 
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @end 
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% @end 
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
