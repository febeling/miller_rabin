
# miller_rabin

An Erlang module implementing the Miller-Rabin test. This test is a
probabilistic test for primality of numbers. It is the alogorithm used
in Mathematica.

The module exports two functions that can be used to check for
primality:

    is_probable_prime(N)

This function uses the original, undeterministic version of the
algorithm. That means it can yield true for a number that is
composite, but this is very rare. (This function tests against K = 20
random bases A.)

    is_prime(N)

This function uses the deterministic variant of the algorithm. It is
reliable for numbers N < 341,550,071,728,321. This version uses not
random sets of bases, like the original version, which could yield
false positives. Instead it uses sets which have been shown to not
yield false positives.

# Links

* http://en.wikipedia.org/wiki/Miller-Rabin_primality_tes
* http://mathworld.wolfram.com/Rabin-MillerStrongPseudoprimeTest.html

# License

MIT.
