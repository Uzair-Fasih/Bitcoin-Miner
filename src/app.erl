-module(app).
-export([start/1, main/3]).

% Keep listening for newer messages from spawned children and incoming workers
listen(NumberOfZeros, ActorCount, NextStart, Prefix) ->
  receive
    {bitcoin, ParentPID, Hash, Input, {_, Time1}, {_, Time2}} -> 
      U1 = Time1 * 1000,
      U2 = Time2 * 1000,

      io:format("~p\tinput:~s\tbitcoin:~s~n", [ParentPID, Input, Hash]),
      io:format("\t\tCode time=~p (~p) microseconds\t ratio=~p~n", [U1,U2, U1 div U2]),      
      listen(NumberOfZeros, ActorCount, NextStart, Prefix);
    
    {connect, WorkerPID} ->
      io:format("~n A new worked connected... ~n~n"),
      WorkerPID ! {ok, NumberOfZeros, NextStart, NextStart + ActorCount, Prefix},
      listen(NumberOfZeros, ActorCount, NextStart + ActorCount + 1, Prefix)
  end.

% The entry point for the server
main(NumberOfZeros, ActorCount, Prefix) ->
  [
    spawn(mine, mineBitcoin, [self(), self(), NumberOfZeros, Prefix, "", Nonce]) ||
    Nonce <- lists:seq(1, ActorCount)
  ],
  listen(NumberOfZeros, ActorCount, ActorCount + 1, Prefix).


% Entry point into the application
start(NumberOfZeros) when is_integer(NumberOfZeros) ->
  {ok, GatorId} = application:get_env(bitcoin, gatorid),
  {ok, ActorCount} = application:get_env(bitcoin, actorcount),
  io:format("Running mining application with prefix: ~s and ~p actors~n~n", [GatorId, ActorCount]),
  register(server, spawn(?MODULE, main, [NumberOfZeros, ActorCount, GatorId]));

% Start a worker instance and connect to server
start(ServerPID) when is_atom(ServerPID)  ->
  {server, ServerPID} ! {connect, self()},
  receive
    {ok, NumberOfZeros, Start, End, Prefix} ->
      [
        spawn(mine, mineBitcoin, [self(), {server, ServerPID}, NumberOfZeros, Prefix, "", Nonce]) ||
        Nonce <- lists:seq(Start, End)
      ]
  end.