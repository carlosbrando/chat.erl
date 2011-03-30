%%%-------------------------------------------------------------------
%%% File:      message_store.erl
%%% @author    Carlos Brando <eduardobrando@gmail.com> [http://www.nomedojogo.com]
%%% @copyright 2011 Carlos Brando
%%% @doc  
%%%
%%% @end  
%%%
%%% @since 2011-03-29 by Carlos Brando
%%%-------------------------------------------------------------------
-module(message_store).
-author('eduardobrando@gmail.com').

-behaviour(gen_server).

-include_lib("stdlib/include/qlc.hrl").

%% API
-export([start_link/0, save_message/2, find_messages/1, shutdown/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define (SERVER, ?MODULE).

-record(state, {}).
-record (chat_message, {addressee, body, created_on}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end 
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
    
save_message (Addressee, Body) ->
  gen_server:call(?SERVER, {save_msg, Addressee, Body}).

find_messages (Addressee) ->
  case gen_server:call(?SERVER, {find_msgs, Addressee}) of
    {ok, Messages} ->
      Messages
  end.

shutdown () ->
  gen_server:call(?SERVER, stop).

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
  process_flag(trap_exit, true),
  io:format("~p (~p) starting...~n", [?MODULE, self()]),
  init_store(),
  {ok, #state{}}.

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
handle_call({save_msg, Addressee, Body}, _From, State) ->
  store_message(Addressee, Body),
  {reply, ok, State};

handle_call({find_msgs, Addressee}, _From, State) ->
  Messages = get_messages(Addressee),
  {reply, {ok, Messages}, State};

handle_call (stop, _From, State) ->
  io:format("Shutting down...~n"),
  mnesia:stop(),
  {stop, normal, State};

handle_call(_Request, _From, State) ->
  {reply, ignored_message, State}.

%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% @doc Handling cast messages
%% @end 
%%--------------------------------------------------------------------
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
init_store () ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  try
    mnesia:table_info(chat_message, type)
  catch
    exit: _ ->
      mnesia:create_table(chat_message, [{attributes, record_info(fields, chat_message)},
                                         {type, bag},
                                         {disc_copies, [node()]}])
  end.
  
get_messages (Addressee) ->
  F = fun () ->
    Query = qlc:q([M || M <- mnesia:table(chat_message), M#chat_message.addressee =:= Addressee]),
    Results = qlc:e(Query),
    delete_messages(Results),
    [Message#chat_message.body || Message <- Results]
  end,
  {atomic, Messages} = mnesia:transaction(F),
  Messages.
  
store_message (Addressee, Body) ->
  F = fun () ->
    {_, CreatedOn, _} = erlang:now(),
    mnesia:write(#chat_message{addressee = Addressee, body = Body, created_on = CreatedOn})
  end,
  mnesia:transaction(F).
  
delete_messages (Messages) ->
  F = fun () ->
    lists:foreach(fun (Msg) -> mnesia:delete_object(Msg) end, Messages)
  end,
  mnesia:transaction(F).