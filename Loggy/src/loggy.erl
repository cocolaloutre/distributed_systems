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
      io:format("\t{~w, ~w, ~p}\n", [From, Time, Msg]),
      New_clock = time:update(From, Time, Clock),
      io:format("\t\tClock: ~w\n", [New_clock]),
      Msgs = lists:keysort(2, lists:append(Msg_queue, [{From, Time, Msg}])),
      Head_msg = nth(1, Msgs),
      case time:safe(element(2, Head_msg), Msgs, New_clock) of
        false ->
          New_msg_queue = Msgs;
        true ->
          log(element(1, Head_msg), element(2, Head_msg), element(3, Head_msg)),
          New_msg_queue = lists:delete(Head_msg, Msgs)
      end,
      loop(New_msg_queue, New_clock);
    stop ->
      ok
  end.

log(From, Time, Msg) ->
  io:format("log: ~w ~w ~p~n", [Time, From, Msg]).
