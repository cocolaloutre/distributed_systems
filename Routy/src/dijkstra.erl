-module(dijkstra).
-export([entry/2, replace/4, update/4, iterate/3, table/2, route/2, sort_list/1]).
-import(map, [reachable/2]).

% entry(Node, Sorted_list) ->
%   This_elem = lists:keyfind(Node, 1, Sorted_list),
%   if
%     This_elem == false ->
%       Elems_with_node = lists:filtermap(fun(Elem) ->
%           if
%             (element(3, Elem) =:= Node) == true ->
%               {true, element(2, Elem)};
%             true ->
%               false
%           end
%         end,
%       Sorted_list),
%       if
%         Elems_with_node == [] ->
%           0;
%         true ->
%           Sorted_elems_with_node = lists:sort(fun(Elem1, Elem2) ->
%               if
%                 element(2, Elem1) =< element(2, Elem2) ->
%                   true;
%                 true ->
%                   false
%               end
%             end,
%             Elems_with_node),
%           lists:nth(1, Sorted_elems_with_node)
%       end;
%     true ->
%       element(2, This_elem)
%   end.

sort_list(List) ->
  lists:sort(fun(Elem1, Elem2) ->
      if
        element(2, Elem1) =< element(2, Elem2) ->
          true;
        true ->
          false
      end
    end,
  List).

entry(Node, Sorted_list) ->
  Elems_with_node = lists:filtermap(fun(Elem) ->
      if
        (element(1, Elem) =:= Node) == true ->
          {true, element(2, Elem)};
        true ->
          false
      end
    end,
  Sorted_list),
  if
    Elems_with_node == [] ->
      0;
    true ->
      lists:nth(1, sort_list(Elems_with_node))
  end.

replace(Node, N, Gateway, Sorted_list) ->
  This_elem = lists:keyfind(Node, 1, Sorted_list),
  if
    This_elem == false ->
      io:format("Node ~w is not in the list.\n", [Node]),
      Sorted_list;
    true ->
      New_list = lists:keydelete(Node, 1, Sorted_list),
      sort_list(lists:append(New_list, [{Node, N, Gateway}]))
  end.

update(Node, N, Gateway, Sorted_list) ->
  This_elem = lists:keyfind(Node, 1, Sorted_list),
  if
    This_elem == false ->
      io:format("Node ~w is not in the list.\n", [Node]),
      Sorted_list;
    N < element(2, This_elem) ->
      New_list = lists:keydelete(Node, 1, Sorted_list),
      sort_list(lists:append(New_list, [{Node, N, Gateway}]));
    true ->
      io:format("A shorter path to ~w exists.\n", [Node]),
      Sorted_list
  end.

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
      % Table_elem = lists:keyfind(Node, 1, Map),
      % if
      %   Table_elem == false ->
      %     New_table = lists:append(Table, [{Node, Node}], [{X, Node} || X <- map:reachable(Node, Map)]),
      %     iterate(Rest, Map, New_table);
      %   true ->
      %     iterate(Rest, Map, Table)

        % true ->
        %   if
        %     N < element(2, Map_elem) ->
        %       iterate(Rest, Map, lists:append(Table, [{Node, N, Gateway}]));
        %     true ->
        %       iterate(update(Node, N, Gateway, Sorted_list), Map, Table)
        %   end
      %end
  end.

table(Gateways, Map) ->
  Gateways_list = lists:foldl(
  fun(Node, List) -> List ++ [{Node, 0, Node}] end,
  [],
  Gateways
  ),
  Map_list = lists:foldl(
  fun(Elem, List) ->
    List ++ [{element(1, Elem), inf, unknown}] ++ [{Reachable, inf, unknown} || Reachable <- element(2, Elem)]
  end,
  [],
  Map
  ),
  Sorted_list = sort_list(Gateways_list ++ Map_list),
  %io:format("Sorted_list: ~w\n", [Sorted_list]),
  iterate(Sorted_list, Map, []).

route(Node, Table) ->
  case lists:keyfind(Node, 1, Table) of
    {_, Gateway} ->
      {ok, Gateway};
    false ->
      notfound
  end.
