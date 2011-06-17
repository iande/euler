module PEHelpers.Primes
(primes) where

-- Based on a solution provided by vineet.  MUCH faster than my seive method.
-- I like it and I need to embrace deeper recursive constructs like this.
isPrime :: Int -> Bool
isPrime n = ipp n primes
  where ipp n (p:ps)
          | p*p > n      = True
          | mod n p == 0 = False
          | otherwise    = ipp n ps

primes = 2 : (filter isPrime [3..])
