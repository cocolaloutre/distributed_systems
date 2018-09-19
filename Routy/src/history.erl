-module(history).
-export([new/1, new/2, add/3, update/3]).

% Creates a new history where messages from Name will always be seen as old
new(Name) ->
  new(Name, inf).

% Creates a new history with message number N
new(Name, N) ->
  [{Name, N}].

% Adds a new entry to the history
add(Name, N, History) ->
  History ++ [{Name, N}].

% Checks if the received message is old or new,
% and updates the message number corresponding to Node if it is new
update(Node, N, History) ->
  case lists:keyfind(Node, 1, History) of
    false ->
      Updated = lists:append(History, [{Node, N}]),
      {new, Updated};
    {_, Old_N} ->
      if
        N =< Old_N ->
          old;
        N > Old_N ->
          Updated = lists:append(lists:keydelete(Node, 1, History), [{Node, N}]),
          {new, Updated}
      end
  end.
