-module(app).
-import(server, [main/1]).
-export([start/1, supervisor/1, performanceMonitor/1]).

% Keep listening for newer messages from spawned children and incoming workers
listen(Nonce, Runtime, {NumberOfZeros, ActorCount, WorkUnit, Prefix}) ->
  receive
    {request_work, RequestorPID} ->
      NextNonce = Nonce + ActorCount,
      RequestorPID ! {ok, Nonce, NextNonce - 1, {NumberOfZeros, ActorCount, WorkUnit, Prefix}},
      listen(NextNonce, Runtime, {NumberOfZeros, ActorCount, WorkUnit, Prefix});

    {bitcoin_found, Input, Hash, {ClientPID, {_, Time}}} -> 
      ElapsedTime = Time * 1000,
      io:format("~p\tinput: ~s\tbitcoin: ~s~n", [ClientPID, Input, Hash]),
      listen(Nonce, Runtime + ElapsedTime, {NumberOfZeros, ActorCount, WorkUnit, Prefix});

    {request_metric, RequestorPID} ->
      RequestorPID ! {metric, Runtime},
      listen(Nonce, 0, {NumberOfZeros, ActorCount, WorkUnit, Prefix})
  end.


% Supervisor spawns the workers for workunits received
supervisor(ServerPID) ->
  ProcessID = self(),
  Supervise = fun Loop() ->
    ServerPID ! {request_work, self()},
    receive
      {ok, Start, End, { NumberOfZeros, ActorCount, WorkUnit, Prefix }} ->
        [
          spawn_link(mine, mineBitcoin, [ProcessID, ServerPID, { NumberOfZeros, WorkUnit / ActorCount, integer_to_list(Nonce), Prefix }]) ||
          Nonce <- lists:seq(Start, End) % Spawns `ActorCount` number of actors and Nonce is always unique
        ],
        Loop()
    end
  end,
  Supervise().

% Performance Monitor prints metrics to the screen after every 10s
performanceMonitor(ServerPID) ->
  Measure = fun Loop() ->
    timer:sleep(10 * 1000),
    ServerPID ! {request_metric, self()},
    receive
      {metric, Runtime} ->
        {_, Time} = statistics(wall_clock),
        ElapsedTime = Time * 1000,
        io:format("CPU Time: ~p, Real Time: ~p, Cores utilised: ~p~n", [Runtime, ElapsedTime, Runtime div ElapsedTime]),
        Loop()
    end
  end,
  Measure().

% The entry point for the server.
% The server spawns two processes: a supervisor and a performance monitor 
% and then listens for messages from workers and processes
start(NumberOfZeros) when is_integer(NumberOfZeros) ->
  {ok, ActorCount} = application:get_env(bitcoin, actorcount),
  {ok, WorkUnit} = application:get_env(bitcoin, workunit),
  {ok, GatorId} = application:get_env(bitcoin, gatorid),
  statistics(wall_clock), % for benchmarking

  Nonce = 1, % Initial Nonce to be used
  Runtime = 0, % Initial runtime
  spawn_link(?MODULE, supervisor, [self()]), % Link supervisor to the main server process 
  spawn_link(?MODULE, performanceMonitor, [self()]), % Link performance monitor to the main server process 

  register(server, self()),
  io:format("Running mining server with prefix: ~s, actors: ~p and workunit ~p.~n", [GatorId, ActorCount, WorkUnit]),

  listen(Nonce, Runtime, {NumberOfZeros, ActorCount, WorkUnit, GatorId});


% The entry point for the client
start(ServerPID) when is_atom(ServerPID)  ->
  io:format("Running mining client connected with ~p.~n", [ServerPID]),
  supervisor({server, ServerPID}).