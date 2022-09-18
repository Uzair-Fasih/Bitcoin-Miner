-module(crypt).
-export([computeHash/1]).

% Compute a hash using the input
computeHash(Input) ->
  io_lib:format("~64.16.0b", [
    binary:decode_unsigned(crypto:hash(sha256, Input))
  ]).
