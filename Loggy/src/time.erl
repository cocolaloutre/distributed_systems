-module(time).
-import(lists, [map/2, keyfind/3, keydelete/3, keyreplace/3, append/2, any/2]).
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

zero() ->
  0.

inc(Name, T) ->
  T + 1.

merge(Ti, Tj) ->
  case leq(Ti, Tj) of
    true ->
      Tj;
    false ->
      Ti
  end.

leq(Ti, Tj) ->
  if
    Ti =< Tj ->
      true;
    true ->
      false
  end.

clock(Nodes) ->
  lists:map(fun(Name) -> {Name, 0} end, Nodes).

update(Node, Time, Clock) ->
  lists:keyreplace(Node, 1, Clock, {Node, Time}).

safe(Time, Clock) ->
  Min_time = lists:min([T || T <- [element(2, Elem) || Elem <- Clock]]),
  if
    Time > Min_time ->
      false;
    true ->
      true
  end.
