%%%-------------------------------------------------------------------
%%% File:      message_router_sup.erl
%%% @author    Carlos Brando <eduardobrando@gmail.com> [http://www.nomedojogo.com]
%%% @copyright 2011 Carlos Brando
%%% @doc  
%%%
%%% @end  
%%%
%%% @since 2011-03-29 by Carlos Brando
%%%-------------------------------------------------------------------
-module(message_router_sup).
-author('eduardobrando@gmail.com').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the supervisor
%% @end 
%%--------------------------------------------------------------------
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% @doc Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%% @end 
%%--------------------------------------------------------------------
init([]) ->
  process_flag(trap_exit, true),
  io:format("~p (~p) starting...~n", [?MODULE, self()]),
  MessageRouter = {message_router, {message_router, start_link, []},
		   permanent, 5000, worker, [message_router]},
  MessageStore = {message_store, {message_store, start_link, []},
		  permanent, 5000, worker, [message_store]},
  {ok, {{one_for_all, 5, 30}, [MessageRouter, MessageStore]}}.

%%====================================================================
%% Internal functions
%%====================================================================
