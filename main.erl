-module(main).
-compile(export_all).

% Compute a hash using the input
computeHash(Input) ->
  io_lib:format("~64.16.0b", [
    binary:decode_unsigned(crypto:hash(sha256, Input))
  ]).

% Continuously mines a bitcoin using tail recursion
mineBitcoin(Pid, ZeroCount, Input, Nonce) -> 
  RegExp = "^0{" ++ integer_to_list(ZeroCount) ++ "}.*",
  Hash = computeHash(Input ++ integer_to_list(Nonce)),
  case re:run(Hash, RegExp) of
    {match, _} -> 
      Pid ! {bitcoin, Hash};
    _ ->
      'better luck next time'
  end,
  mineBitcoin(Pid, ZeroCount, Hash, Nonce).

listen() ->
  receive
    {bitcoin, Hash} -> 
      io:format("~s~n", [Hash])
  end,
  listen().

% The entry into the project
start(ZeroCount, ProcessCount) ->
  GatorId = "mfasih;",
  [
    spawn(?MODULE, mineBitcoin, [self(), ZeroCount, GatorId, X]) ||
    X <- lists:seq(1,ProcessCount)
  ],
  listen().

start(ZeroCount) ->
  start(ZeroCount, 3).