
# miller_rabin

An Erlang module implementing the Miller-Rabin test. This test is a
probabilistic test for primality of numbers. It is the alogrithm used
in Mathematica.

The module exports three functions that can be used to check for
primality:

    is_probable_prime(N)

and

    is_probable_prime(N, K)

These functions use the original, undeterministic version of the
algorithm. That means it can yield true for a number that is
composite, but this is very rare. The first function tests against K =
20 random bases per default.

    is_prime(N)

This function uses the deterministic variant of the algorithm. It is
reliable for numbers N < 341,550,071,728,321. This version uses not
random sets of bases, like the original version, which could yield
false positives. Instead it uses sets which have been shown to not
yield false positives for the intervals in which the function is
defined.

# Example Usage

This example prints all primes up to 1000.

  -module(example).

  -import(miller_rabin, [is_prime/1]).

  -compile([export_all]).

  primes_below_1000() ->
      L = lists:seq(1, 1000),
      lists:map(fun(X) ->
                        case is_prime(X) of
                            true  -> io:format("~w~n", [X]);
                            false -> false
                        end
                end,
                L),
      ok.

# Links

* http://en.wikipedia.org/wiki/Miller-Rabin_primality_tes
* http://mathworld.wolfram.com/Rabin-MillerStrongPseudoprimeTest.html

# License

MIT.
