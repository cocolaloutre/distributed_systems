-module(gms4).
-export([start/1, start/2]).
-define(timeout, 100).
-define(drop, 100).
-define(arghh, 1000).

start(Id) ->
  Rnd = random:uniform(1000),
  Self = self(),
  {ok, spawn_link(fun()-> init(Id, Rnd, Self) end)}.

init(Id, Rnd, Master) ->
  random:seed(Rnd, Rnd, Rnd),
  leader(Id, Master, 0, [], [Master], []).

start(Id, Grp) ->
  Rnd = random:uniform(1000),
  Self = self(),
  {ok, spawn_link(fun()-> init(Id, Rnd, Grp, Self) end)}.

init(Id, Rnd, Grp, Master) ->
  random:seed(Rnd, Rnd, Rnd),
  Self = self(),
  Grp ! {join, Master, Self},
  receive
    {view, N, [Leader|Slaves], Group} ->
      erlang:monitor(process, Leader),
      Master ! {view, Group},
      slave(Id, Master, Leader, N+1, {view, N, [Leader|Slaves], Group}, Slaves, Group)
    after ?timeout ->
      Master ! {error, "no reply from leader"}
  end.

leader(Id, Master, N, Slaves, Group, Msgs) ->
  receive
    {mcast, Msg} ->
      bcast(Id, {msg, N, Msg}, Slaves),
      New_msgs = [{N, Msg} | Msgs],
      Master ! Msg,
      leader(Id, Master, N+1, Slaves, Group, New_msgs);
    {join, Wrk, Peer} ->
      Slaves2 = lists:append(Slaves, [Peer]),
      Group2 = lists:append(Group, [Wrk]),
      bcast(Id, {view, N, [self()|Slaves2], Group2}, Slaves2),
      Master ! {view, Group2},
      leader(Id, Master, N+1, Slaves2, Group2, Msgs);
    {resend, I, Peer} ->
      case lists:keyfind(I, 1, Msgs) of
        {_, Msg} ->
          io:format("Leader resends message ~w to peer ~w\n", [I, Peer]),
          Peer ! {resend, I, Msg};
        false ->
          % If we can't find the message, it means that the requested message
          % is a view message.
          % Instead of storing the view messages as well, we just send the
          % most up-to-date view.
          Peer ! {view, I, Slaves, Group}
      end,
      leader(Id, Master, N, Slaves, Group, Msgs);
    stop ->
      ok
  end.

slave(Id, Master, Leader, N, Last, Slaves, Group) ->
  receive
    {mcast, Msg} ->
      Leader ! {mcast, Msg},
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {join, Wrk, Peer} ->
      Leader ! {join, Wrk, Peer},
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {msg, I, _} when I < N ->
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {msg, N, Msg} ->
      Master ! Msg,
      slave(Id, Master, Leader, N+1, {msg, N, Msg}, Slaves, Group);
    {msg, I, Msg} -> % I > N so we have not received some messages
      Msg_nbs = lists:seq(N, I-1),
      io:format("Slave ~w has not received messages ~w\n", [Id, Msg_nbs]),
      lists:map(fun(Nb) -> Leader ! {resend, Nb, self()} end, Msg_nbs),
      Master ! Msg,
      slave(Id, Master, Leader, I+1, {msg, I, Msg}, Slaves, Group);
    {resend, I, Msg} ->
      io:format("Slave ~w has finally received message ~w\n", [Id, I]),
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {view, I, [Leader|Slaves2], Group2} ->
      Master ! {view, Group2},
      slave(Id, Master, Leader, I+1, {view, I, [Leader|Slaves2], Group2}, Slaves2, Group2);
    {view, I, [Leader2|Slaves2], Group2} ->
      erlang:monitor(process, Leader2),
      Master ! {view, Group2},
      slave(Id, Master, Leader2, I+1, {view, I, [Leader2|Slaves2], Group2}, Slaves2, Group2);
    {'DOWN', _Ref, process, Leader, _Reason} ->
      election(Id, Master, N, Last, Slaves, Group);
    stop ->
      ok;
    Error ->
      io:format("Message received but ignored: ~w~n", [Error])
  end.

election(Id, Master, N, Last, Slaves, [_|Group]) ->
  Self = self(),
  case Slaves of
    [Self|Rest] ->
      bcast(Id, Last, Rest),
      bcast(Id, {view, N, Slaves, Group}, Rest),
      Master ! {view, Group},
      leader(Id, Master, N+1, Rest, Group, [element(1, Last), element(2, Last)]);
    [Leader|Rest] ->
      erlang:monitor(process, Leader),
      slave(Id, Master, Leader, N, Last, Rest, Group)
  end.

bcast(Id, Msg, Nodes) ->
  lists:foreach(fun(Node) -> send_msg(Node, Msg), crash(Id) end, Nodes).

send_msg(Node, Msg) ->
	case random:uniform(?drop) of
		?drop ->
			io:format("Message (~w) dropped for node ~w\n", [Msg, Node]),
			drop;
		_ ->
			Node ! Msg
  end.

crash(Id) ->
  case random:uniform(?arghh) of
    ?arghh ->
      io:format("leader ~w: crash~n", [Id]),
      exit(no_luck);
    _ ->
      ok
  end.
