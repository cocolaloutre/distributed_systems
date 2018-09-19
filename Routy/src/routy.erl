-module(routy).
-export([start/2, stop/1, displayStatus/1]).

% Starts a new router process
start(Reg, Name) ->
  register(Reg, spawn(fun() -> init(Name) end)).

% Initiates the parameters of the router
init(Name) ->
  Interfaces = interface:new(),
  Map = map:new(),
  Table = dijkstra:table(Interfaces, Map),
  History = history:new(Name, 0),
  router(Name, 0, History, Interfaces, Table, Map).

% Stops the router process
stop(Node) ->
  Node ! stop,
  unregister(Node).

router(Name, N, History, Interfaces, Table, Map) ->
  receive
    % Adds a new Node to the interfaces
    {add, Node, Pid} ->
      Ref = erlang:monitor(process,Pid),
      New_interfaces = interface:add(Node, Ref, Pid, Interfaces),
      io:format("Node ~w added\n", [Node]),
      router(Name, N, History, New_interfaces, Table, Map);

    % Removes Node from the interfaces
    {remove, Node} ->
      {ok, Ref} = interface:ref(Node, Interfaces),
      erlang:demonitor(Ref),
      New_interfaces = interface:remove(Node, Interfaces),
      io:format("Node ~w removed\n", [Node]),
      router(Name, N, History, New_interfaces, Table, Map);

    % Removes the node of ref Ref from the interfaces after it is down
    {'DOWN', Ref, process, _, _} ->
      {ok, Down} = interface:name(Ref, Interfaces),
      io:format("~w: exit received from ~w\n", [Name, Down]),
      New_interfaces = interface:remove(Down, Interfaces),
      router(Name, N, History, New_interfaces, Table, Map);

    % Replies the status to From
    {status, From} ->
      From ! {status, {Name, N, History, Interfaces, Table, Map}},
      router(Name, N, History, Interfaces, Table, Map);

    % Shuts down
    stop ->
      ok;

    % Updates the interfaces and the map according to the information received
    {links, Node, R, Links} ->
      case history:update(Node, R, History) of
        {new, New_history} ->
          io:format("New history: ~w\n", [New_history]),
          interface:broadcast({links, Node, R, Links}, Interfaces),
          New_map = map:update(Node, Links, Map),
          io:format("New map: ~w\n", [New_map]),
          router(Name, N, New_history, Interfaces, Table, New_map);
        old ->
          router(Name, N, History, Interfaces, Table, Map)
      end;

    % Updates the table
    update ->
      New_table = dijkstra:table(interface:list(Interfaces), Map),
      io:format("New table: ~w\n", [New_table]),
      router(Name, N, History, Interfaces, New_table, Map);

    % Broadcasts the known information
    broadcast ->
      Message = {links, Name, N, interface:list(Interfaces)},
      interface:broadcast(Message, Interfaces),
      router(Name, N+1, History, Interfaces, Table, Map);

    % Receives a message
    {route, Name, _, Message} ->
      io:format("~w: received message ~w\n", [Name, Message]),
      router(Name, N, History, Interfaces, Table, Map);

    % Routes a message that is aimed at another process
    {route, To, From, Message} ->
      io:format("~w: routing message (~w)\n", [Name, Message]),
      case dijkstra:route(To, Table) of
        {ok, Gateway} ->
          case interface:lookup(Gateway, Interfaces) of
            {ok, Pid} ->
              io:format("Message passing through ~w\n", [Gateway]),
              Pid ! {route, To, From, Message};
            notfound ->
              io:format("Message droped\n"),
              ok
          end;
        notfound ->
          io:format("Message droped\n"),
          ok
      end,
      router(Name, N, History, Interfaces, Table, Map);

    % Sends the Message
    {send, To, Message} ->
      self() ! {route, To, Name, Message},
      router(Name, N, History, Interfaces, Table, Map)
  end.

% Displays the status of the router
displayStatus(Router) ->
    Router ! {status, self()},
    receive
  		{status, {Name, N, History, Interfaces, Table, Map}} ->
  			io:format("Status:\n"),
  			io:format("\tname: ~w\n", [Name]),
  			io:format("\tN: ~w\n", [N]),
  			io:format("\thistory: ~w\n", [History]),
  			io:format("\tinterfaces: ~w\n", [interface:list(Interfaces)]),
  			io:format("\ttable: ~w\n", [Table]),
  			io:format("\tmap: ~w\n", [Map]),
  			ok;
  		Else ->
  			io:format("Received: ~w\n", [Else])
    end.
