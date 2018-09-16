-module(dijkstra).
-export([entry/2, replace/4]).

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
      Sorted_elems_with_node = lists:sort(fun(Elem1, Elem2) ->
          if
            element(2, Elem1) =< element(2, Elem2) ->
              true;
            true ->
              false
          end
        end,
        Elems_with_node),
      lists:nth(1, Sorted_elems_with_node)
  end.

replace(Node, N, Gateway, Sorted_list) ->
  Sorted_list.
