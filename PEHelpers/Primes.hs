module PEHelpers.Primes
(primes) where

-- Based on a solution provided by vineet.  MUCH faster than my seive method.
-- I like it and I need to embrace deeper recursive constructs like this.

-- (d:ds) must be in ascending order for this to work
isPrime [] n = True
isPrime (d:ds) n
  | d*d > n      = True
  | mod n d == 0 = False
  | otherwise    = isPrime ds n

primes = 2 : (filter (isPrime primes) [3..])
