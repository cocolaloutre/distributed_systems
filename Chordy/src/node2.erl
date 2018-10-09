-module(node2).
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
  node(Id, Predecessor, Successor, []).

connect(Id, nil) ->
  {ok, {Id, self()}};

connect(Id, Peer) ->
  Qref = make_ref(),
  Peer ! {key, Qref, self()},
  receive
    {Qref, Skey} ->
      {ok, {Skey, Peer}}
    after ?Timeout ->
      io:format("Time out: no response~n",[])
  end.

node(Id, Predecessor, Successor, Store) ->
  receive
    {key, Qref, Peer} ->
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor, Store);
    {notify, New} ->
      Pred = notify(New, Id, Predecessor, Store),
      node(Id, Pred, Successor, Store);
    {request, Peer} ->
      request(Peer, Predecessor),
      node(Id, Predecessor, Successor, Store);
    {status, Pred} ->
      Succ = stabilize(Pred, Id, Successor),
      node(Id, Predecessor, Succ, Store);
    stabilize ->
      stabilize(Successor),
      node(Id, Predecessor, Successor, Store);
    state ->
      io:format("State of node ~w:\n", [Id]),
      io:format("\tPredecessor: ~w\n", [Predecessor]),
      io:format("\tSuccessor: ~w\n", [Successor]),
      io:format("\tStore: ~w\n", [Store]),
      node(Id, Predecessor, Successor, Store);
    probe ->
      create_probe(Id, Successor),
      node(Id, Predecessor, Successor, Store);
    {probe, Id, Nodes, T} ->
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor, Store);
    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor),
      node(Id, Predecessor, Successor, Store);
    {add, Key, Value, Qref, Client} ->
      Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Added);
    {lookup, Key, Qref, Client} ->
      lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Store);
    {handover, Elements} ->
      Merged = storage:merge(Store, Elements),
      node(Id, Predecessor, Successor, Merged);
    stop ->
      stop;
    Error ->
      io:format("Strange message: ~w\n", [Error]),
      node(Id, Predecessor, Successor, Store)
  end.

schedule_stabilize() ->
  timer:send_interval(?Stabilize, self(), stabilize).

stabilize({_, Spid}) ->
  Spid ! {request, self()}.

stabilize(Pred, Id, Successor) ->
  {Skey, Spid} = Successor,
  case Pred of
    nil -> % our successor doesn't have a predecessor, we notify it about us
      Spid ! {notify, {Id, self()}},
      Successor;
    {Id, _} -> % we are already our successor's predecessor
      Successor;
    {Skey, _} -> % our successor's predecessor is itself, so we notify it about us
      Spid ! {notify, {Id, self()}},
      Successor;
    {Xkey, Xpid} -> % our successor's predecessor is another node
      case key:between(Xkey, Id, Skey) of
        true -> % this node is between us and our successor
          stabilize({Xkey, Xpid}),
          Pred;
        false -> % we should be between the node and our successor
          Spid ! {notify, {Id, self()}},
          Successor
      end
  end.

request(Peer, Predecessor) ->
  case Predecessor of
    nil ->
      Peer ! {status, nil};
    {Pkey, Ppid} ->
      Peer ! {status, {Pkey, Ppid}}
  end.

notify({Nkey, Npid}, Id, Predecessor, Store) ->
  case Predecessor of
    nil ->
      handover(Id, Store, Nkey, Npid),
      {Nkey, Npid};
    {Pkey, _} ->
      case key:between(Nkey, Pkey, Id) of
        true ->
          handover(Id, Store, Nkey, Npid),
          {Nkey, Npid};
        false ->
          Predecessor
      end
  end.

create_probe(Id, Successor) ->
  T = erlang:system_time(microsecond),
  {_, Spid} = Successor,
  Spid ! {probe, Id, [Id], T}.

forward_probe(Ref, T, Nodes, Id, Successor) ->
  {_, Spid} = Successor,
  %io:format("nodes: ~w\n", [Nodes ++ [Id]]),
  Spid ! {probe, Ref, Nodes ++ [Id], T}.

remove_probe(T, Nodes) ->
  Time = erlang:system_time(microsecond) - T,
  io:format("Time: ~w\n", [Time]),
  io:format("Nodes: ~w\n", [Nodes]).

add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
  case key:between(Key, Pkey, Id) of
    true ->
      Client ! {Qref, ok},
      Store ++ [{Key, Value}];
    false ->
      Spid ! {add, Key, Value, Qref, Client},
      Store
  end.

lookup(Key, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
  case key:between(Key, Pkey, Id) of
    true ->
      Result = storage:lookup(Key, Store),
      Client ! {Qref, Result};
    false ->
      Spid ! {lookup, Key, Qref, Client}
  end.

handover(Id, Store, Nkey, Npid) ->
  {Keep, Rest} = storage:split(Id, Nkey, Store),
  Npid ! {handover, Rest},
  Keep.
