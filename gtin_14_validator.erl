-module (gtin_14_validator).

-export ([validate/1]).

-spec
validate ({gtin, Value :: iolist() | binary()}) -> ok | {error, term()}.
validate ({gtin, Value}) ->
  try
    Digits = [ list_to_integer([D]) || <<D>> <= iolist_to_binary(Value) ],
    case length(Digits) of
      14 -> ok;
      _L -> error(invalid_length)
    end,
    validate(Digits, 0)
  catch
    error:E -> {error, E};
    _:_     -> {error, unknown_error}
  end;
validate (_) ->
  {error, badarg}.

validate ([CheckDigit], Acc) ->
  case (10 - (Acc rem 10)) rem 10 of
    CheckDigit -> ok;
    _ -> {error, invalid_check_digit}
  end;
validate ([D, CheckDigit], Acc) ->
  validate([CheckDigit], Acc + D*3);
validate ([D1, D2 | T], Acc) ->
  validate(T, Acc + D1*3 + D2).
