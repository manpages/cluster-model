-module(clusterm_sup).

-behaviour(supervisor).

-export([ init/1
         ,start_link/0 ]).

start_link() ->
  Variable = self(),
  io:format("start_link: Hey, I'm process number ~p~n", [Variable]),
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  io:format("init: Hey, I'm process number ~p~n", [self()]),
  {ok, {{one_for_all, 5, 60}, []}}.
