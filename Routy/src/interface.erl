-module(interface).
-import(lists, [keyfind/3, keydelete/3, append/2, foldl/3, foreach/2]).
-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).

new() ->
  [].

add(Name, Ref, Pid, Set) ->
  lists:append(Set, [{Name, Ref, Pid}]).

remove(Name, Set) ->
  lists:keydelete(Name, 1, Set).

lookup(Name, Set) ->
  case lists:keyfind(Name, 1, Set) of
    {_, _, Pid} ->
      {ok, Pid};
    false ->
      notfound
  end.

ref(Name, Set) ->
  case lists:keyfind(Name, 1, Set) of
    {_, Ref, _} ->
      {ok, Ref};
    false ->
      notfound
  end.

name(Ref, Set) ->
  case lists:keyfind(Ref, 2, Set) of
    {Name, _, _} ->
      {ok, Name};
    false ->
      notfound
  end.

list(Set) ->
  lists:foldl(fun(Interface, List) -> List ++ [element(1, Interface)] end,
  [],
  Set).

broadcast(Message, Set) ->
  lists:foreach(fun({_, _, Pid}) -> Pid ! Message end,
  Set).
