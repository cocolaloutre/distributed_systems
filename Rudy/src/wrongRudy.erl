-module(wrongRudy).
-export([start/1, start/2, stop/0]).

% Starts the server
start(Port) ->
  start(Port, 1).
start(Port, NbHandlers) ->
  register(rudy, spawn(fun() -> init(Port, NbHandlers) end)).

% Stops the server
stop() ->
  exit(whereis(rudy), "time to die").

% Creates a socket and the handlers in case of a connection
init(Port, NbHandlers) ->
  Opt = [list, {active, false}, {reuseaddr, true}],
  case gen_tcp:listen(Port, Opt) of
    {ok, Listen} ->
      spawnHandlers(Listen, NbHandlers, self()),
      checkHandlers(NbHandlers),
      %handler(Listen),
      gen_tcp:close(Listen),
      ok;
    {error, Error} ->
      io:format("Init error: ~w~n", [Error])
  end.

% Creates multiple handler processes
spawnHandlers(Listen, NbHandlers, Pid) ->
  case NbHandlers of
    0 ->
      ok;
    _ ->
      spawn(fun() -> handler(Listen, Pid) end),
      %spawn(rudy, handler, [Listen, Pid]),
      %handler(Listen),
      spawnHandlers(Listen, NbHandlers-1, Pid)
  end.

% Checks if the handlers are done
checkHandlers(NbHandlers) ->
  case NbHandlers of
    0 ->
      ok;
    _ ->
      receive
        done ->
          checkHandlers(NbHandlers-1)
      end,
  end.

% Handles the connection
handler(Listen, Pid) ->
  case gen_tcp:accept(Listen) of
    {ok, Client} ->
      request(Client),
      handler(Listen, Pid);
    {error, Error} ->
      io:format("Handler error: ~w~n", [Error])
  end,
  Pid ! done.

% Handles the client request
request(Client) ->
  Recv = gen_tcp:recv(Client, 0),
  case Recv of
    {ok, Str} ->
      Request = http:parse_request(Str),
      Response = reply(Request),
      gen_tcp:send(Client, Response);
    {error, Error} ->
      io:format("Request error: ~w~n", [Error])
  end,
  gen_tcp:close(Client).

% Replies to the client
reply({{get, URI, _}, _, _}) ->
  %io:format("Request received for ~s~n", [URI]),
  timer:sleep(40),
  http:ok(URI).
