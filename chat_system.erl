-module(chat_system).
-author('eduardobrando@gmail.com').

-behaviour(application).

-export([start/0, start/2, stop/1]).

start () ->
  mnesia:start(),
  application:start(?MODULE).
  
start(_Type, _StartArgs) ->
  message_router_sup:start_link().

stop(_State) ->
  ok.
