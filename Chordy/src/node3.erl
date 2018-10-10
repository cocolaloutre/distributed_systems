-module(node3).
-export([start/1, start/2]).
-define(Stabilize, 1000).
-define(Timeout, 1000).

start(Id) ->
  start(Id, nil).

start(Id, Peer) ->
  timer:start(),
  spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
  io:format("New node ~w, pid: ~w\n", [Id, self()]),
  Predecessor = nil,
  {ok, Successor} = connect(Id, Peer),
  schedule_stabilize(),
  node(Id, Predecessor, Successor, nil, storage:create()).

connect(Id, nil) ->
  {ok, {Id, nil, self()}};
connect(Id, Peer) ->
  Qref = make_ref(),
  Peer ! {key, Qref, self()},
  receive
    {Qref, Skey} ->
      Ref = monitor(Peer),
      {ok, {Skey, Ref, Peer}}
    after ?Timeout ->
      io:format("Node ~w: timeout, no response~n",[Id])
  end.

node(Id, Predecessor, Successor, Next, Store) ->
  receive
    {key, Qref, Peer} ->
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor, Next, Store);
    {notify, New} ->
      {Pred, Keep} = notify(New, Id, Predecessor, Store),
      node(Id, Pred, Successor, Next, Keep);
    {request, Peer} ->
      request(Peer, Predecessor, Successor),
      node(Id, Predecessor, Successor, Next, Store);
    {status, Pred, Nx} ->
      {Succ, Nxt} = stabilize(Pred, Nx, Id, Successor),
      node(Id, Predecessor, Succ, Nxt, Store);
    stabilize ->
      stabilize(Successor),
      node(Id, Predecessor, Successor, Next, Store);
    state ->
      io:format("State of node ~w:\n", [Id]),
      io:format("\tPredecessor: ~w\n", [Predecessor]),
      io:format("\tSuccessor: ~w\n", [Successor]),
      io:format("\tStore: ~w\n", [Store]),
      node(Id, Predecessor, Successor, Next, Store);
    probe ->
      create_probe(Id, Successor),
      node(Id, Predecessor, Successor, Next, Store);
    {probe, Id, Nodes, T} ->
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor, Next, Store);
    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor),
      node(Id, Predecessor, Successor, Next, Store);
    {add, Key, Value, Qref, Client} ->
      Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Next, Added);
    {lookup, Key, Qref, Client} ->
      lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Next, Store);
    {handover, Elements} ->
      Merged = storage:merge(Store, Elements),
      node(Id, Predecessor, Successor, Next, Merged);
    {'DOWN', Ref, process, _, _} ->
      {Pred, Succ, Nxt} = down(Ref, Predecessor, Successor, Next),
      node(Id, Pred, Succ, Nxt, Store);
    stop ->
      stop;
    Error ->
      io:format("Strange message: ~w\n", [Error]),
      node(Id, Predecessor, Successor, Next, Store)
  end.

schedule_stabilize() ->
  timer:send_interval(?Stabilize, self(), stabilize).

stabilize({_, _, Spid}) ->
  Spid ! {request, self()}.

stabilize(Pred, Next, Id, Successor) ->
  {Skey, Sref, Spid} = Successor,
  case Pred of
    nil -> % our successor doesn't have a predecessor, we notify it about us
      Spid ! {notify, {Id, nil, self()}},
      {Successor, Next};
    {Id, _, _} -> % we are already our successor's predecessor
      {Successor, Next};
    {Skey, _, _} -> % our successor's predecessor is itself, so we notify it about us
      Spid ! {notify, {Id, nil, self()}},
      {Successor, Next};
    {Xkey, _, Xpid} -> % our successor's predecessor is another node
      case key:between(Xkey, Id, Skey) of
        true -> % this node is between us and our successor
          self() ! stabilize,
          drop(Sref),
          Xref = monitor(Xpid),
          {{Xkey, Xref, Xpid}, Successor};
        false -> % we should be between the node and our successor
          Spid ! {notify, {Id, nil, self()}},
          {Successor, Next}
      end
  end.

request(Peer, Predecessor, Successor) ->
  case Predecessor of
    nil ->
      Peer ! {status, nil, Successor};
    {Pkey, Pref, Ppid} ->
      Peer ! {status, {Pkey, Pref, Ppid}, Successor}
  end.

notify({Nkey, _, Npid}, Id, Predecessor, Store) ->
  case Predecessor of
    nil ->
      Keep = handover(Id, Store, Nkey, Npid),
      Nref = monitor(Npid),
      {{Nkey, Nref, Npid}, Keep};
    {Pkey, Pref, _} ->
      case key:between(Nkey, Pkey, Id) of
        true ->
          Keep = handover(Id, Store, Nkey, Npid),
          drop(Pref),
          Nref = monitor(Npid),
          {{Nkey, Nref, Npid}, Keep};
        false ->
          {Predecessor, Store}
      end
  end.

create_probe(Id, Successor) ->
  T = erlang:system_time(microsecond),
  {_, _, Spid} = Successor,
  Spid ! {probe, Id, [Id], T}.

forward_probe(Ref, T, Nodes, Id, Successor) ->
  {_, _, Spid} = Successor,
  %io:format("nodes: ~w\n", [Nodes ++ [Id]]),
  Spid ! {probe, Ref, Nodes ++ [Id], T}.

remove_probe(T, Nodes) ->
  Time = erlang:system_time(microsecond) - T,
  io:format("Time: ~w\n", [Time]),
  io:format("Nodes: ~w\n", [Nodes]).

add(Key, Value, Qref, Client, Id, {Pkey, _, _}, {_, _, Spid}, Store) ->
  case key:between(Key, Pkey, Id) of
    true ->
      Client ! {Qref, ok},
      Store ++ [{Key, Value}];
    false ->
      Spid ! {add, Key, Value, Qref, Client},
      Store
  end.

lookup(Key, Qref, Client, Id, {Pkey, _, _}, {_, _, Spid}, Store) ->
  case key:between(Key, Pkey, Id) of
    true ->
      Result = storage:lookup(Key, Store),
      Client ! {Qref, Result};
    false ->
      Spid ! {lookup, Key, Qref, Client}
  end.

handover(Id, Store, Nkey, Npid) ->
  {Keep, Rest} = storage:split(Nkey, Id, Store),
  Npid ! {handover, Rest},
  Keep.

monitor(Pid) ->
  erlang:monitor(process, Pid).

drop(nil) ->
  ok;
drop(Ref) ->
  erlang:demonitor(Ref, [flush]).

down(Ref, {_, Ref, _}, Successor, Next) ->
  {nil, Successor, Next};
down(Ref, Predecessor, {_, Ref, _}, {Nkey, _, Npid}) ->
  Nref = monitor(Npid),
  self() ! stabilize,
  {Predecessor, {Nkey, Nref, Npid}, nil};
down(_, Predecessor, Successor, Next) ->
  {Predecessor, Successor, Next}.
