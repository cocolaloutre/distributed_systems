-module(worker).
-import(time, [zero/0, inc/2, merge/2, leq/2]).
-export([start/5, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter) ->
  spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
  Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
  random:seed(Seed, Seed, Seed),
  receive
    {peers, Peers} ->
      loop(Name, Log, Peers, Sleep, Jitter, time:zero());
    stop ->
      ok
  end.

peers(Wrk, Peers) ->
  Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter, Time)->
  Wait = random:uniform(Sleep),
  receive
    {msg, Peer_time, Msg} ->
      New_time = time:inc(Name, time:merge(Time, Peer_time)),
      Log ! {log, Name, New_time, {received, Msg}},
      loop(Name, Log, Peers, Sleep, Jitter, New_time);
    stop ->
      ok;
    Error ->
      Log ! {log, Name, time, {error, Error}}
    after Wait ->
      Selected = select(Peers),
      Message = {hello, random:uniform(100)},
      New_time = time:inc(Name, Time),
      Selected ! {msg, New_time, Message},
      jitter(Jitter),
      Log ! {log, Name, New_time, {sending, Message}},
      loop(Name, Log, Peers, Sleep, Jitter, New_time)
  end.

select(Peers) ->
  lists:nth(random:uniform(length(Peers)), Peers).

jitter(0) -> ok;
  jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).
