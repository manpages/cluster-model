-module(clusterm_cluster_sup).

-behaviour(supervisor).

%% Behaviour exports
-export([ init/1
         ,start_link/0 ]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  %% Read more about ChildSpec and simple_one_for_one supervisors here:
  %% http://www.erlang.org/doc/design_principles/sup_princ.html#spec
  ChildSpec = [{clusterm_node, {clusterm_node, start_link, []}, 
                temporary, brutal_kill, worker, [clusterm_node]}]
  {ok, {{simple_one_for_one, 5, 60}, ChildSpec}}.
