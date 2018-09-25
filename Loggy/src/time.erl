-module(time).
-export([zero/0, inc/2, merge/2, leq/2]).

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
