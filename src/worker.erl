-module(worker).
-export([start/1]).

% Start a worker instance and connect to server
start(ServerPID) ->
  {server, ServerPID} ! {connect, self()},
  receive
    {ok, NumberOfZeros, Start, End, Prefix} ->
      [
        spawn(mine, mineBitcoin, [self(), {server, ServerPID}, NumberOfZeros, Prefix, "", Nonce]) ||
        Nonce <- lists:seq(Start, End)
      ]
  end.