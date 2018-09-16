-module(dijkstra).
-export([entry/2, replace/4, update/4, sort_list/1]).

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
      io:format("Node ~w is not in the list.\n", [Node]);
    true ->
      New_list = lists:keydelete(Node, 1, Sorted_list),
      sort_list(lists:append(New_list, [{Node, N, Gateway}]))
  end.

update(Node, N, Gateway, Sorted_list) ->
  This_elem = lists:keyfind(Node, 1, Sorted_list),
  if
    This_elem == false ->
      io:format("Node ~w is not in the list.\n", [Node]);
    N < element(2, This_elem) ->
      New_list = lists:keydelete(Node, 1, Sorted_list),
      sort_list(lists:append(New_list, [{Node, N, Gateway}]));
    true ->
      io:format("A shorter path to ~w exists.\n", [Node])
  end.
