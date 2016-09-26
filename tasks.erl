-module (tasks).

-export ([task/1]).

-define (DEBUG, ok).

-compile (export_all).

-define (FIB_MAX, 4000000).

-ifdef (DEBUG).
-define (DBG (F, A), io:format (standard_error, "* ~s:~w: " ++ F, [?MODULE, ?LINE | A])).
-else.
-define (DBG (F, A), ok).
-endif.

-define (in_range(N, Min, Max),
         is_integer(N) andalso (N >= Min) andalso (N =< Max)).

-define (int_3_digits(N), ?in_range(N, 100, 999)).

%%======================================================================
%% Tasks
%%======================================================================

task (1) -> sum_of_mult(1, 1000, [3, 5]);
task (2) -> fib_even_sum(1, 2, 0);
task (3) -> largest_prime_factor(600851475143);
task (4) -> find_largest_palindrom();
task (5) -> least_common_multiple(lists:seq(1, 20));
task (_) -> {error, not_implemented}.

%%======================================================================
%% Private
%%======================================================================

sum_of_mult (Min, Max, List = [_|_])
  when is_integer(Min) andalso
       is_integer(Max) andalso
       (Min > 0)       andalso
       (Max > 0)       andalso
       (Min =< Max) ->
  sum_of_mult (Min, Max, List, 0);
sum_of_mult (_, _, _) ->
  error(badarg).

sum_of_mult (Max, Max, _, Sum) ->
  Sum;
sum_of_mult (N, Max, List, Sum) ->
  case lists:any(fun
                   (K) when is_integer(K) -> N rem K == 0;
                   (_) -> error(badarg)
                 end,
                 List) of
    true  -> sum_of_mult(N + 1, Max, List, Sum + N);
    false -> sum_of_mult(N + 1, Max, List, Sum)
  end.

%------------------------------------------------------------------------

fib_even_sum (_, Fcurr, Sum) when Fcurr > ?FIB_MAX ->
  Sum;
fib_even_sum (Fprev, Fcurr, Sum) when Fcurr rem 2 == 0 ->
  fib_even_sum(Fcurr, Fprev + Fcurr, Sum + Fcurr);
fib_even_sum (Fprev, Fcurr, Sum) ->
  fib_even_sum(Fcurr, Fprev + Fcurr, Sum).

%------------------------------------------------------------------------

largest_prime_factor (1) ->
  io:format(
    "~nPrime number is the number that has excplicitly 2 divisors.~n"
    "The number \"1\" has only 1 divisor, so this is not a prime number.~n~n"),
  throw(badarg);
largest_prime_factor (2) ->
  ?DBG("Largest Prime Factor of 2 is 2~n", []),
  2;
largest_prime_factor (3) ->
  ?DBG("Largest Prime Factor of 3 is 3~n", []),
  3;
largest_prime_factor (N) when N rem 2 == 0 ->
  ?DBG(
    "Largest Prime Factor of ~p is "
    "Largest Prime Factor of ~p because ~p is even~n", [N, N div 2, N]),
  largest_prime_factor(N div 2);
largest_prime_factor (N) when N rem 3 == 0 ->
  ?DBG(
    "Largest Prime Factor of ~p is "
    "Largest Prime Factor of ~p because ~p is divided by 3~n", [N, N div 3, N]),
  largest_prime_factor(N div 3);
largest_prime_factor (N) ->
  CtrlFactor = math:sqrt(N),
  Truncated = trunc(CtrlFactor),
  case Truncated == CtrlFactor of
    true ->
      ?DBG("Largest Prime Factor of ~p is "
           "Largest Prime Factor of ~p because ~p is square~n",
           [N, Truncated, N]),
      largest_prime_factor(Truncated);
    _ ->
      largest_prime_factor(N, Truncated + 1)
  end.

largest_prime_factor (N, CtrlFactor) ->
  {McrSec, Value} =
    timer:tc(fun () -> largest_prime_factor(N, 1, CtrlFactor, 1) end),
  ?DBG("Value: ~p, time: ~f sec~n", [Value, McrSec/1000000]),
  Value.

largest_prime_factor (N, K, CtrlFactor, CurrMax) ->
  largest_prime_factor(N, K, 6*K-1, 6*K+1, CtrlFactor, CurrMax).

largest_prime_factor (N, _, Smaller, _, CtrlFactor, _)
  when CtrlFactor =< Smaller andalso N rem Smaller == 0 ->
  ?DBG("Largest Prime Factor of ~p is ~p~n", [N, Smaller]),
  Smaller;
largest_prime_factor (N, _, Smaller, _, CtrlFactor, 1)
  when CtrlFactor =< Smaller ->
  ?DBG("Largest Prime Factor of ~p is ~p~n", [N, N]),
  N;
largest_prime_factor (_N, _, Smaller, _, CtrlFactor, CurrMax)
  when CtrlFactor =< Smaller ->
  ?DBG("Largest Prime Factor of ~p is ~p~n", [_N, CurrMax]),
  CurrMax;
largest_prime_factor (N, _, _, Bigger, CtrlFactor, _)
  when CtrlFactor =< Bigger andalso N rem Bigger == 0 ->
  ?DBG("Largest Prime Factor of ~p is ~p~n", [N, Bigger]),
  Bigger;
largest_prime_factor (N, _, _, Bigger, CtrlFactor, 1)
  when CtrlFactor =< Bigger ->
  ?DBG("Largest Prime Factor of ~p is ~p~n", [N, N]),
  N;
largest_prime_factor (_N, _, _, Bigger, CtrlFactor, CurrMax)
  when CtrlFactor =< Bigger ->
  ?DBG("Largest Prime Factor of ~p is ~p~n", [_N, CurrMax]),
  CurrMax;
largest_prime_factor (N, K, _, Bigger, CtrlFactor, _)
  when N rem Bigger == 0 ->
  ?DBG("Largest Prime Factor of ~p is "
       "Largest Prime Factor of ~p because ~p is divided by ~p~n",
       [N, N div Bigger, N, Bigger]),
  max(Bigger, largest_prime_factor(N div Bigger, K, CtrlFactor, 1));
largest_prime_factor (N, K, Smaller, _, CtrlFactor, _)
  when N rem Smaller == 0 ->
  ?DBG("Largest Prime Factor of ~p is "
       "Largest Prime Factor of ~p because ~p is divided by ~p~n",
       [N, N div Smaller, N, Smaller]),
  max(Smaller, largest_prime_factor(N div Smaller, K, CtrlFactor, 1));
largest_prime_factor (N, K, _S, _B, CtrlFactor, CurrMax) ->
  largest_prime_factor(N, K + 1, CtrlFactor, CurrMax).

%------------------------------------------------------------------------

-spec find_largest_palindrom() -> {ok, integer()} | {error, not_found}.
find_largest_palindrom () ->
  case find_largest_palindrom(fun make_palindrom_6_digits/3) of
    {ok, Value} -> Value;
    _ ->
      case find_largest_palindrom(fun make_palindrom_5_digits/3) of
        {ok, Value} -> Value;
        Error       -> Error
      end
  end.

find_largest_palindrom(Fun) ->
  find_largest_palindrom(Fun, {9,9,9}).

find_largest_palindrom (_, {0, _, _}) ->
  {error, not_found};
find_largest_palindrom (Fun, Digits = {X, Y, Z}) ->
  Number = Fun(X, Y, Z),
  case find_closest_factor(Number) of
    {M, N} when ?int_3_digits(M) andalso ?int_3_digits(N) -> {ok, Number};
    _ -> find_largest_palindrom(Fun, dec(Digits))
  end.

%%
%% X = M*N, lim |M-N| -> 0
%%
-spec find_closest_factor (X :: integer()) -> {M :: integer(), N :: integer()}.
find_closest_factor (0) ->
  {0, 0};
find_closest_factor (X) when is_integer(X) ->
  PerfectVal = round(math:sqrt(X)),
  find_closest_factor(X, PerfectVal);
find_closest_factor (_) ->
  error(badarg).

find_closest_factor (X, Value) when X rem Value == 0 -> {Value, X div Value};
find_closest_factor (X, Value) -> find_closest_factor (X, Value - 1).


-spec make_palindrom_6_digits (X :: 1..9, Y :: 0..9, Z :: 0..9) -> 100001..999999.
make_palindrom_6_digits (X, Y, Z) when is_integer(X) andalso
                                       is_integer(Y) andalso
                                       is_integer(Z) andalso
                                       ?in_range(X, 1, 9) andalso
                                       ?in_range(Y, 0, 9) andalso
                                       ?in_range(Z, 0, 9) ->
  100001*X + 10010*Y + 1100*Z;
make_palindrom_6_digits(_, _, _) ->
  error(badarg).


-spec make_palindrom_5_digits (X :: 1..9, Y :: 0..9, Z :: 0..9) -> 100001..999999.
make_palindrom_5_digits (X, Y, Z) when is_integer(X) andalso
                                       is_integer(Y) andalso
                                       is_integer(Z) andalso
                                       ?in_range(X, 1, 9) andalso
                                       ?in_range(Y, 0, 9) andalso
                                       ?in_range(Z, 0, 9) ->
  10001*X + 1010*Y + 100*Z;
make_palindrom_5_digits(_, _, _) ->
  error(badarg).


-spec dec({integer(), integer(), integer()}) -> {integer(), integer(), integer()}.
dec ({0, 0, 0}) -> {0, 0, 0};
dec ({X, 0, 0}) -> {X-1, 9, 9};
dec ({X, Y, 0}) -> {X, Y-1, 9};
dec ({X, Y, Z}) -> {X, Y, Z-1};
dec (_) -> error(badarg).

%------------------------------------------------------------------------

gcd (N, 0) -> N;
gcd (M, N) when M > N -> gcd(N, M rem N);
gcd (M, N) when M < N -> gcd(M, N rem M).

lcm (M, N) ->
  M*N div gcd(M, N).

-spec least_common_multiple ([integer()]) -> integer().
least_common_multiple ([]) ->
  error(badarg);
least_common_multiple (List) ->
  lists:foldl(fun lcm/2, hd(List), tl(List)).

