-module(map).
-import(lists, [keyfind/3, keydelete/3, append/2, foldl/3]).
-export([new/0, add/2, update/3, reachable/2, all_nodes/1]).

% Here, Elem designates an element of the map like {Node, Links}

% Creates an empty map
new() ->
  [].

% Adds an element to the map (if there's already an element for the same node,
% updates its links)
add(Elem, Map) ->
  This_elem = lists:keyfind(element(1, Elem), 1, Map),
  if
    This_elem == false ->
      lists:append(Map, [Elem]);
    true ->
      update(element(1, Elem), element(2, Elem), Map)
  end.

% Updates the links of Node
update(Node, Links, Map) ->
  Old_elem = lists:keyfind(Node, 1, Map),
  if
    Old_elem /= false ->
      New_map = lists:keydelete(Node, 1, Map);
    true ->
      New_map = Map
  end,
  New_elem = {Node, Links},
  add(New_elem, New_map).

% Returns a list of nodes in direct communication with Node
reachable(Node, Map) ->
  Elem = lists:keyfind(Node, 1, Map),
  if
    Elem /= false ->
      element(2, Elem);
    true ->
      []
  end.

% Returns all the nodes, included the ones without outgoing links
all_nodes(Map) ->
  lists:usort(lists:flatmap(fun(Elem) -> [element(1, Elem)] ++ element(2, Elem) end, Map)).
