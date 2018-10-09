-module(key).
-export([generate/0, between/3]).

generate() ->
  random:uniform(1000000000).

between(Key, From, To) ->
  if
    From == To ->
      true;
    To > From ->
      if
        (Key > From) and (Key =< To) ->
          true;
        true ->
          false
      end;
    From > To ->
      if
        (Key > From) or (Key < To) ->
          true;
        true ->
          false
      end;
    Key == To ->
      To;
    true ->
      false
  end.
