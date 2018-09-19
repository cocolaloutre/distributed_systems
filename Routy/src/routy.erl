-module(routy).
-export([start/2, stop/1, displayStatus/1]).

start(Reg, Name) ->
  register(Reg, spawn(fun() -> init(Name) end)).

init(Name) ->
  Interfaces = interface:new(),
  Map = map:new(),
  Table = dijkstra:table(Interfaces, Map),
  History = history:new(Name),
  router(Name, 0, History, Interfaces, Table, Map).

stop(Node) ->
  Node ! stop,
  unregister(Node).

router(Name, N, History, Interfaces, Table, Map) ->
  receive
    {add, Node, Pid} ->
      Ref = erlang:monitor(process,Pid),
      New_interfaces = interface:add(Node, Ref, Pid, Interfaces),
      io:format("Node ~w added\n", [Node]),
      router(Name, N, History, New_interfaces, Table, Map);

    {remove, Node} ->
      {ok, Ref} = interface:ref(Node, Interfaces),
      erlang:demonitor(Ref),
      New_interfaces = interface:remove(Node, Interfaces),
      io:format("Node ~w removed\n", [Node]),
      router(Name, N, History, New_interfaces, Table, Map);

    {'DOWN', Ref, process, _, _} ->
      {ok, Down} = interface:name(Ref, Interfaces),
      io:format("~w: exit received from ~w~n", [Name, Down]),
      New_interfaces = interface:remove(Down, Interfaces),
      router(Name, N, History, New_interfaces, Table, Map);

    {status, From} ->
      From ! {status, {Name, N, History, Interfaces, Table, Map}},
      router(Name, N, History, Interfaces, Table, Map);

    stop ->
      ok;

    {links, Node, R, Links} ->
      case history:update(Node, R, History) of
        {new, New_history} ->
          io:format("New history: ~w\n", [New_history]),
          interface:broadcast({links, Node, R, Links}, Interfaces),
          New_map = map:update(Node, Links, Map),
          router(Name, N, New_history, Interfaces, Table, New_map);
        old ->
          router(Name, N, History, Interfaces, Table, Map)
      end;

    update ->
      New_table = dijkstra:table(interface:list(Interfaces), Map),
      io:format("New table: ~w\n", [New_table]),
      router(Name, N, History, Interfaces, New_table, Map);

    broadcast ->
      Message = {links, Name, N, interface:list(Interfaces)},
      interface:broadcast(Message, Interfaces),
      router(Name, N+1, History, Interfaces, Table, Map);

    {route, Name, _, Message} ->
      io:format("~w: received message ~w ~n", [Name, Message]),
      router(Name, N, History, Interfaces, Table, Map);

    {route, To, From, Message} ->
      io:format("~w: routing message (~w)", [Name, Message]),
      case dijkstra:route(To, Table) of
        {ok, Gateway} ->
          case interface:lookup(Gateway, Interfaces) of
            {ok, Pid} ->
              Pid ! {route, To, From, Message};
            notfound ->
              ok
          end;
        notfound ->
          ok
      end,
      router(Name, N, History, Interfaces, Table, Map);

    {send, To, Message} ->
      self() ! {route, To, Name, Message},
      router(Name, N, History, Interfaces, Table, Map)
  end.

displayStatus(Router) ->
    Router ! {status, self()},
    receive
  		{status, {Name, N, Hist, Intf, Table, Map}} ->
  			io:format("Status:\n"),
  			io:format("\tname: ~w\n", [Name]),
  			io:format("\tn: ~w\n", [N]),
  			io:format("\tmsgs: ~w\n", [Hist]),
  			io:format("\tinterfaces: ~w\n", [Intf]),
  			io:format("\ttable: ~w\n", [Table]),
  			io:format("\tmap: ~w\n", [Map]),
  			ok;
  		Else ->
  			io:format("Received: ~w\n", [Else])
    end.
