-module(history).
-export([new/1, new/2, add/3, update/3]).

new(Name) ->
  new(Name, inf).

new(Name, N) ->
  [{Name, N}].

add(Name, N, History) ->
  History ++ [{Name, N}].

update(Node, N, History) ->
  case lists:keyfind(Node, 1, History) of
    false ->
      lists:append(History, [{Node, N}]);
    {_, Old_N} ->
      if
        N =< Old_N ->
          old;
        N > Old_N ->
          Updated = lists:append(lists:keydelete(Node, 1, History), [{Node, N}]),
          {new, Updated}
      end
  end.
