-module(interface).
-import(lists, [keyfind/3, keydelete/3, append/2, foldl/3, foreach/2]).
-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).

% Creates an empty set of interfaces
new() ->
  [].

% Adds a new interface to the set
add(Name, Ref, Pid, Set) ->
  lists:append(Set, [{Name, Ref, Pid}]).

% Removes an interface from the set
remove(Name, Set) ->
  lists:keydelete(Name, 1, Set).

% Returns the Pid of the interface of name Name, if found
lookup(Name, Set) ->
  case lists:keyfind(Name, 1, Set) of
    {_, _, Pid} ->
      {ok, Pid};
    false ->
      notfound
  end.

% Returns the reference of the interface of name Name, if found
ref(Name, Set) ->
  case lists:keyfind(Name, 1, Set) of
    {_, Ref, _} ->
      {ok, Ref};
    false ->
      notfound
  end.

% Returns the name of the interface of reference Ref, if found
name(Ref, Set) ->
  case lists:keyfind(Ref, 2, Set) of
    {Name, _, _} ->
      {ok, Name};
    false ->
      notfound
  end.

% Returns the whole list of interface names
list(Set) ->
  lists:foldl(
    fun(Interface, List) -> List ++ [element(1, Interface)] end,
    [],
    Set
  ).

% Sends a Message to all the interfaces in the set
broadcast(Message, Set) ->
  lists:foreach(
    fun({_, _, Pid}) -> Pid ! Message end,
    Set
  ).
