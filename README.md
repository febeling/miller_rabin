
# miller_rabin

Test Miller-Rabin test for is a probabilistic test for primality of
numbers. It is the alogorithm used in Mathematica.

It exports two functions that can be used to check for primality:

  is_probable_prime(N)

This function uses the original, undeterministic version of the
algorithm. That means it can yield true for a number that is
composite, but this is very rare.

  is_prime(N)

This function uses the deterministic variant of the algorithm. It
reliable for number N < 341,550,071,728,321. This version uses not
random sets of bases, like the original version. Instead it uses sets
which have been shown to not yield false positives.

# Links

* http://en.wikipedia.org/wiki/Miller-Rabin_primality_tes
* http://mathworld.wolfram.com/Rabin-MillerStrongPseudoprimeTest.html

# License

MIT.
