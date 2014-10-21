-module(clusterm_app).

-behaviour(application).

-export([ start/0
         ,start/2
         ,stop/1 ]).

start() ->
  application:start(clusterm).

start(_, _) ->
  io:format("Hey, I'm application and I like starting supervisors and my process id is ~p~n", [self()]),
  clusterm_sup:start_link().

stop(_) ->
  ok.
