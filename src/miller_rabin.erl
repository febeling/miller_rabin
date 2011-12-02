%% Fast primality test
%%
%% Deterministic Test (specific known bases prove primality), see:
%% http://primes.utm.edu/prove/prove2_3.html
%%
-module(miller_rabin).

-export([is_prime/1, is_probable_prime/1, below_1000/0, pow_mod/3]).

basis(N) when N>2 ->
    1 + random:uniform(N-2).

find_ds(D, S) when D rem 2 == 0 ->
    find_ds(D div 2, S+1);
find_ds(D, S) ->
    {D, S}.

find_ds(N) ->
    find_ds(N-1, 0).

pow_mod(B, E, M) when is_integer(B),
                      is_integer(E),
                      is_integer(M) ->
    case E of
        0 -> 1;
        E when E rem 2 == 0 ->
            trunc(math:pow(pow_mod(B, E div 2, M), 2)) rem M;
        _Else ->
            trunc(B*pow_mod(B, E-1, M)) rem M
    end.

mr_sequence(N, A, D, S) when N rem 2 == 1 ->
    Js = lists:seq(0, S),
    lists:map(fun(J) ->
                      case pow_mod(A, trunc(math:pow(2, J)*D), N) of
                          X when X == N-1 -> -1;
                          X -> X
                      end
              end,
              Js).

is_composite_sequence(Xs) ->
    case Xs of
        [1|_] -> false;
        L     -> not lists:member(-1, L)
    end.

is_mr_prime(N, As) when N>2, N rem 2 == 1 ->
    {D, S} = find_ds(N),
    Ss = lists:map(fun(A) -> mr_sequence(N, A, D, S) end, As),
    not lists:any(fun(Seq) -> is_composite_sequence(Seq) end, Ss).

proving_bases(N) when N == 3 ->
    [2];
proving_bases(N) when N < 1373653 ->
    [2, 3];
proving_bases(N) when N < 25326001 ->
    [2, 3, 5];
proving_bases(N) when N < 25000000000 ->
    [2, 3, 5, 7];
proving_bases(N) when N < 2152302898747->
    [2, 3, 5, 7, 11];
proving_bases(N) when N < 3474749660383 ->
    [2, 3, 5, 7, 11, 13];
proving_bases(N) when N < 341550071728321 ->
    [2, 3, 5, 7, 11, 13, 17].

random_bases(N, K) ->
    [basis(N) || _ <- lists:seq(1, K)].

is_prime(1) -> false;
is_prime(2) -> true;
is_prime(N) when N rem 2 == 0 -> false;
is_prime(N) when N < 341550071728321 ->
    is_mr_prime(N, proving_bases(N)).

is_probable_prime(N, K) ->
    is_mr_prime(N, random_bases(N, K)).

is_probable_prime(N) ->
    is_probable_prime(N, 20).
