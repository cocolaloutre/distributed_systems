-module(loggy).
-import(lists, [append/2, nth/2, keysort/2, delete/2]).
-import(time, [clock/1, update/3, safe/2]).
-export([start/1, stop/1]).

start(Nodes) ->
  spawn_link(fun() ->init(Nodes) end).

stop(Logger) ->
  Logger ! stop.

init(Nodes) ->
  Clock = time:clock(Nodes),
  loop([], Clock).

loop(Msg_queue, Clock) ->
  receive
    {log, From, Time, Msg} ->
      %io:format("\t{~w, ~w, ~p}\n", [From, Time, Msg]),
      Msgs = lists:keysort(2, lists:append(Msg_queue, [{From, Time, Msg}])),
      New_msg_queue = log_safe_msgs(Msgs, Clock),
      %io:format("\t\t  Msg_queue: ~w\n", [New_msg_queue]),
      New_clock = time:update(From, Time, Clock),
      %io:format("\t\tClock: ~w\n", [New_clock]),
      loop(New_msg_queue, New_clock);
    stop ->
      ok
  end.

log_safe_msgs(Msg_queue, Clock) ->
  case Msg_queue of
    [{From, Time, Msg} | Rest] ->
      case time:safe(Time, Clock) of
        false ->
          Msg_queue;
        true ->
          log(From, Time, Msg),
          log_safe_msgs(Rest, Clock)
      end;
    [] ->
      []
  end.


log(From, Time, Msg) ->
  io:format("\nlog: ~w ~w ~p\n\n", [Time, From, Msg]).
