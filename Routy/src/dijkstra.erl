-module(dijkstra).
-export([table/2, route/2]).
-import(map, [reachable/2]).

% Sorts the list based on the path length
sort_list(List) ->
  lists:sort(
    fun(Elem1, Elem2) ->
      if
        element(2, Elem1) =< element(2, Elem2) ->
          true;
        true ->
          false
      end
    end,
    List
  ).

% Returns the length of the shortest path to the node
% or 0 if the node is not found
entry(Node, Sorted_list) ->
  Elems_with_node = lists:filtermap(
    fun(Elem) ->
      if
        (element(1, Elem) =:= Node) == true ->
          {true, element(2, Elem)};
        true ->
          false
      end
    end,
    Sorted_list
  ),
  if
    Elems_with_node == [] ->
      0;
    true ->
      lists:nth(1, sort_list(Elems_with_node))
  end.

% Replaces the entry for Node in Sorted_list
% with a new entry having a new length N and Gateway
replace(Node, N, Gateway, Sorted_list) ->
  This_elem = lists:keyfind(Node, 1, Sorted_list),
  if
    This_elem == false ->
      % io:format("Node ~w is not in the list.\n", [Node]),
      Sorted_list;
    true ->
      New_list = lists:keydelete(Node, 1, Sorted_list),
      sort_list(lists:append(New_list, [{Node, N, Gateway}]))
  end.

% Updates the list Sorted given knowing that
% Node can be reached in N hops using Gateway
update(Node, N, Gateway, Sorted_list) ->
  This_elem = lists:keyfind(Node, 1, Sorted_list),
  if
    This_elem == false ->
      % io:format("Node ~w is not in the list.\n", [Node]),
      Sorted_list;
    N < element(2, This_elem) ->
      replace(Node, N, Gateway, Sorted_list);
    true ->
      % io:format("A shorter path to ~w exists.\n", [Node]),
      Sorted_list
  end.

% Builds the routing table, given a Map and a Sorted_list of nodes
iterate(Sorted_list, Map, Table) ->
  case Sorted_list of
    [] ->
      Table;
    [{_, inf, _} | _] ->
      Table;
    [{Node, N, Gateway} | Rest] ->
      case lists:keyfind(Node, 1, Map) of
        false ->
          New_list = Rest;
        _ ->
          %io:format("Reachables of ~w: ~w\n", [Node, map:reachable(Node, Map)]),
          New_list = lists:foldl(
            fun(Temp_node, S) -> update(Temp_node, N+1, Gateway, S) end,
            Rest,
            map:reachable(Node, Map)
          )
      end,
      %io:format("New_list: ~w\n", [New_list]),
      %io:format("Table: ~w\n", [Table]),
      iterate(New_list, Map, [{Node, Gateway} | Table])
  end.

% Builds a routing table, given the Gateways and the Map
table(Gateways, Map) ->
  Nodes = map:all_nodes(Map),
  Direct_nodes = lists:map(
    fun(Temp_node) -> {Temp_node, 0, Temp_node} end,
    Gateways
  ),
  Rest = lists:filter(
    fun(Temp_node) -> not lists:member(Temp_node, Gateways) end,
    Nodes
  ),
  Indirect_nodes = lists:map(
    fun(Temp_node) -> {Temp_node, inf, unknown} end,
    Rest
  ),
  Sorted_list = lists:append(Direct_nodes, Indirect_nodes),
  iterate(Sorted_list, Map, []).

% Returns the gateway suitable to route messages to Node if it is found
route(Node, Table) ->
  case lists:keyfind(Node, 1, Table) of
    {_, Gateway} ->
      {ok, Gateway};
    false ->
      notfound
  end.
