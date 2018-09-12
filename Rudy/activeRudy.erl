-module(activeRudy).

-export([start/1, start/2, stop/0, fib/1, request/1]).

start(Port) ->
  start(Port, 1).

start(Port, N) ->
  register(rudy, spawn(fun() -> init(Port, N) end)).

stop() ->
  rudy ! stop.

init(Port, N) ->
  case gen_tcp:listen(Port, [list, {active, true}, {reuseaddr, true}]) of
    {ok, Listen} ->
      handlers(Listen, N),
      super(Listen);
    {error, Error} ->
      io:format("rudy: initialization failed: ~w~n", [Error]),
      error
  end.

super(Socket) ->
  receive
    stop ->
      %gen_tcp:controlling_process(Socket, self()),
      gen_tcp:close(Socket),
      ok
  end.

handlers(Listen, N) ->
  case N of
    0 ->
      ok;
    N ->
      spawn(fun() -> handler(Listen, N) end),
      handlers(Listen, N-1)
  end.



handler(Listen, I) ->
  %%io:format("rudy: waiting for request~n", []),
  case gen_tcp:accept(Listen) of
    {ok, Client} ->
      io:format("rudy ~w: received request~n", [I]),
      request(Client),
      handler(Listen, I);
    {error, Error} ->
      super(Listen),
      io:format("rudy: error ~w~n", [Error]),
      error
  end.

request(Client) ->
  receive
    stop ->
      ok;
    {tcp, Client, Str} ->
      Request = http:parse_request(Str),
      Response = reply(Request),
      gen_tcp:send(Client, Response),
      gen_tcp:close(Client);
    {error, Error} ->
      super(Client),
      io:format("rudy: error: ~w~n", [Error]),
      ok
  end.


reply({{get, URI, _}, _, _}) ->
  timer:sleep(40),
  %%fib(30),
  http:ok("<html><head><title>Rudy</title></head><body>This is a test.<br/>" ++ URI ++ "</body></html>").


fib(N) ->
  if
    N == 0 ->
      0;
    N == 1 ->
      1;
    true -> fib(N-1) + fib(N-2)
  end.
