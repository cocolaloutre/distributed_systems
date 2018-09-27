-module(time).
-import(lists, [map/2, keyfind/3, keydelete/3, keyreplace/4, append/2, any/2]).
-export([zero/0, inc/2, merge/3, clock/1, update/3, safe/2]).

zero() ->
  0.

inc(Name, VClock) ->
  case lists:keyfind(Name, 1, VClock) of
		{N, T} ->
			lists:keyreplace(Name, 1, VClock, {N, T + 1});
		false ->
			[{Name, 1} | VClock]
  end.

merge(Name, Self_clock, Peer_clock) ->
  case Peer_clock of
    [{Name, _} | Rest] ->
      merge(Name, Self_clock, Rest);
    [{Node, Time} | Rest] ->
      case (Time > time(Node, Self_clock)) of
        true ->
          New_self_clock = update(Node, Time, Self_clock),
          merge(Name, New_self_clock, Rest);
        false ->
          merge(Name, Self_clock, Rest)
      end;
    [] ->
      Self_clock
  end.

clock(Nodes) ->
  lists:map(fun(Name) -> {Name, 0} end, Nodes).

update(Node, Time, Clock) ->
  lists:keyreplace(Node, 1, Clock, {Node, Time}).

safe(VClock, Clock) ->
  case VClock of
    [{Node, Time} | Rest] ->
      case (Time > time(Node, Clock)) of
        true ->
          false;
        false ->
          safe(Rest, Clock)
      end;
    [] ->
      true
  end.

time(Node, Clock) ->
  element(2, lists:keyfind(Node, 1, Clock)).
