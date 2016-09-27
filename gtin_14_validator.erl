-module (gtin_14_validator).

-export ([validate/1]).

-compile(export_all).

validate (N) ->
  CheckDigit = N rem 10,

digits (N) ->
  digits(N, []).

digits (0, [])     -> [0];
digits (0, Digits) -> Digits;
digits (N, Digits) -> digits(N div 10, [N rem 10 | Digits]).
