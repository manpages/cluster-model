-module(clusterm_just_recursion).

-export([ for_is_just_recursion/1
         ,dialyzer_error_example/0 ]).

%% Uncomment this function, then run
%% make dialyze
%% To get the following error:
%% clusterm_just_recursion.erl:6: Function dialyzer_error_example/0 has no local return
%% clusterm_just_recursion.erl:7: The call clusterm_just_recursion:for_is_just_recursion('sad_panda') 
%%                                will never return since it differs in the 1st argument from the success 
%%                                typing arguments: (non_neg_integer())
% dialyzer_error_example() ->
%  for_is_just_recursion(sad_panda).

for_is_just_recursion(X) ->
  for_is_just_recursion(X, 1).

% private functions:

for_is_just_recursion(0, Acc) ->
  {ok, Acc};
for_is_just_recursion(X, Acc) ->
  for_is_just_recursion(X-1, Acc*X).
