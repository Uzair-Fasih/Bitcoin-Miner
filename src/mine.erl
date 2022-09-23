-module(mine).
-export([mineBitcoin/3]).

% Computes a base64 hash using SHA256 from the input
computeHash(Input) ->
  io_lib:format("~64.16.0b", [
    binary:decode_unsigned(crypto:hash(sha256, Input))
  ]).


% Continuously mines a bitcoin using tail recursion 
% until the work unit is exhausted and 
% sends all found bitcoins to a given process
mineBitcoin(ClientPID, ServerPID, { NumberOfZeros, WorkUnit, PrevHash, Prefix }) ->

  Input = Prefix ++ PrevHash,
  Hash = computeHash(Input),
  RegExp = "^0{" ++ integer_to_list(NumberOfZeros) ++ "}.*",
  
  case re:run(Hash, RegExp) of
    {match, _} -> 
      % Return ClientPID, runtime for benchmarking/book-keeping
      ServerPID ! {bitcoin_found, Input, Hash, {ClientPID, statistics(runtime)}}; 
    _ ->
      'better luck next time'
  end,
  
  % Compute more cycles until the work unit is exhausted
  if WorkUnit > 0 -> mineBitcoin(ClientPID, ServerPID, { NumberOfZeros, WorkUnit - 1, Hash, Prefix });
    true -> 'Done'
  end.