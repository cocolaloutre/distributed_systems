-module(storage).
-export([create/0, add/3, lookup/2, split/3, merge/2]).

create() ->
  [].

add(Key, Value, Store) ->
  Store ++ [{Key, Value}].

lookup(Key, Store) ->
  lists:keyfind(Key, 1, Store).

split(From, To, Store) ->
  lists:splitwith(
    fun(Key) ->
      key:between(Key, From, To)
    end,
    Store
  ).

merge(Entries, Store) ->
  Store ++ Entries.
