-module(clusterm_just_recursion).

-export([ for_is_just_recursion/1 ]).

for_is_just_recursion(X) ->
  for_is_just_recursion(X, 1).

% private functions:

for_is_just_recursion(0, Acc) ->
  {ok, Acc};
for_is_just_recursion(X, Acc) ->
  for_is_just_recursion(X-1, Acc*X).
