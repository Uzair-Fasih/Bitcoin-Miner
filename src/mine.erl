-module(mine).
-export([mineBitcoin/6]).

% Computes a base64 hash using SHA256 from the input
computeHash(Input) ->
  io_lib:format("~64.16.0b", [
    binary:decode_unsigned(crypto:hash(sha256, Input))
  ]).

% Continuously mines a bitcoin using tail recursion and 
% sends found bitcoin to a given process
mineBitcoin(ParentPID, MainPid, NumberOfZeros, Prefix, Input, Nonce) ->
  GeneratedInput = Prefix ++ Input ++ integer_to_list(Nonce),
  Hash = computeHash(GeneratedInput),
  RegExp = "^0{" ++ integer_to_list(NumberOfZeros) ++ "}.*",
  case re:run(Hash, RegExp) of
    {match, _} -> 
      MainPid ! {bitcoin, ParentPID, Hash, GeneratedInput, statistics(runtime), statistics(wall_clock)};
    _ ->
      'better luck next time'
  end,
  mineBitcoin(ParentPID, MainPid, NumberOfZeros, Prefix, Hash, Nonce).