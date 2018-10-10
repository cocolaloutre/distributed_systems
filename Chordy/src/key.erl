-module(key).
-export([generate/0, generate/1, between/3]).

generate() ->
  random:uniform(1000000000).

generate(Rnd) ->
  random:seed(Rnd, Rnd, Rnd),
  random:uniform(1000000000).

between(Key, From, To) ->
  if
    From == To ->
      true;
    Key == To ->
      true;
    To > From ->
      (Key > From) and (Key =< To);
    From > To ->
      (Key > From) or (Key < To);
    true ->
      false
  end.
