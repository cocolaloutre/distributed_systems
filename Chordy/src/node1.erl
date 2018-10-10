-module(node1).
-export([start/1, start/2]).
-define(Stabilize, 1000).
-define(Timeout, 1000).

start(Id) ->
  start(Id, nil).

start(Id, Peer) ->
  io:format("New node ~w\n", [Id]),
  timer:start(),
  spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
  Predecessor = nil,
  {ok, Successor} = connect(Id, Peer),
  io:format("Successor: ~w\n", [Successor]),
  schedule_stabilize(),
  node(Id, Predecessor, Successor).

connect(Id, nil) ->
  {ok, {Id, self()}};
connect(Id, Peer) ->
  Qref = make_ref(),
  Peer ! {key, Qref, self()},
  receive
    {Qref, Skey} ->
      {ok, {Skey, Peer}}
    after ?Timeout ->
      io:format("Node ~w: timeout, no response~n",[Id])
  end.

node(Id, Predecessor, Successor) ->
  receive
    {key, Qref, Peer} ->
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor);
    {notify, New} ->
      Pred = notify(New, Id, Predecessor),
      node(Id, Pred, Successor);
    {request, Peer} ->
      request(Peer, Predecessor),
      node(Id, Predecessor, Successor);
    {status, Pred} ->
      Succ = stabilize(Pred, Id, Successor),
      node(Id, Predecessor, Succ);
    stabilize ->
      stabilize(Successor),
      node(Id, Predecessor, Successor);
    state ->
      io:format("State of node ~w:\n", [Id]),
      io:format("\tPredecessor: ~w\n", [Predecessor]),
      io:format("\tSuccessor: ~w\n", [Successor]),
      node(Id, Predecessor, Successor);
    probe ->
      create_probe(Id, Successor),
      node(Id, Predecessor, Successor);
    {probe, Id, Nodes, T} ->
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor);
    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor),
      node(Id, Predecessor, Successor);
    stop ->
      stop;
    Error ->
      io:format("Strange message: ~w\n", [Error]),
      node(Id, Predecessor, Successor)
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
    {Xkey, _} -> % our successor's predecessor is another node
      case key:between(Xkey, Id, Skey) of
        true -> % this node is between us and our successor
          self() ! stabilize,
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

notify({Nkey, Npid}, Id, Predecessor) ->
  case Predecessor of
    nil ->
      {Nkey, Npid};
    {Pkey, _} ->
      case key:between(Nkey, Pkey, Id) of
        true ->
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
