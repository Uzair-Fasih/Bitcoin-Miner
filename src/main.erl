-module(main).
-compile(export_all).

% Compute a hash using the input
computeHash(Input) ->
  io_lib:format("~64.16.0b", [
    binary:decode_unsigned(crypto:hash(sha256, Input))
  ]).

% Continuously mines a bitcoin using tail recursion
mineBitcoin(Pid, ZeroCount, GatorId, Input, Nonce, ParentPID) -> 
  RegExp = "^0{" ++ integer_to_list(ZeroCount) ++ "}.*",
  ComputedInput = GatorId ++ Input ++ integer_to_list(Nonce),
  Hash = computeHash(ComputedInput),
  case re:run(Hash, RegExp) of
    {match, _} -> 
      Pid ! {bitcoin, ParentPID, Hash, ComputedInput};
    _ ->
      'better luck next time'
  end,
  mineBitcoin(Pid, ZeroCount, GatorId, Hash, Nonce, ParentPID).

% Keep listening for newer messages from spawned children
listen(NextNonce, ProcessCount, ZeroCount) ->
  receive
    {bitcoin, WorkerName, Hash, Input} -> 
      io:format("~p \t found bitcoin: ~s using input ~s~n", [WorkerName, Hash, Input]),
      listen(NextNonce, ProcessCount, ZeroCount);
    {connect, WorkerPID} ->
      io:format("A new worked connected~n"),
      WorkerPID ! {ok, NextNonce, NextNonce + ProcessCount, ZeroCount},
      listen(NextNonce + ProcessCount + 1, ProcessCount, ZeroCount)
  end.

start_worker(BossPID) ->
  GatorId = "mfasih;",
  {start, BossPID} ! {connect, self()},
  receive
    {ok, Start, ProcessCount, ZeroCount} ->
      [
        spawn(?MODULE, mineBitcoin, [{start, BossPID}, ZeroCount, GatorId, "", X, self()]) ||
        X <- lists:seq(Start, ProcessCount)
      ]
  end.

% The entry into the project
start(ZeroCount, ProcessCount) ->
  GatorId = "mfasih;",
  [
    spawn(?MODULE, mineBitcoin, [self(), ZeroCount, GatorId, "", X, self()]) ||
    X <- lists:seq(1, 1 + ProcessCount)
  ],
  listen(ProcessCount + 1, ProcessCount, ZeroCount).

start(ZeroCount) ->
  register(start, spawn(main, start, [ZeroCount, 8])).