-module(server).
-export([start/2, main/3]).

% Keep listening for newer messages from spawned children and incoming workers
listen(NumberOfZeros, ActorCount, NextStart, Prefix) ->
  receive
    {bitcoin, ParentPID, Hash, Input} -> 
      io:format("~p\t found: ~s input ~s~n", [ParentPID, Hash, Input]),
      listen(NumberOfZeros, ActorCount, NextStart, Prefix);
    {connect, WorkerPID} ->
      io:format("A new worked connected...~n"),
      WorkerPID ! {ok, NumberOfZeros, NextStart, NextStart + ActorCount, Prefix},
      listen(NumberOfZeros, ActorCount, NextStart + ActorCount + 1, Prefix)
  end.

% The entry into the project
main(NumberOfZeros, ActorCount, Prefix) ->
  [
    spawn(mine, mineBitcoin, [self(), self(), NumberOfZeros, Prefix, "", Nonce]) ||
    Nonce <- lists:seq(1, ActorCount)
  ],
  listen(NumberOfZeros, ActorCount, ActorCount + 1, Prefix).


start(NumberOfZeros, ActorCount) ->
  {ok, GatorId} = application:get_env(bitcoin, gatorid),
  register(server, spawn(?MODULE, main, [NumberOfZeros, ActorCount, GatorId])).