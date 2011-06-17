{--
By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see
that the 6th prime is 13.

What is the 10001st prime number?
--}

reject :: (a -> Bool) -> [a] -> [a]
reject f = filter (not . f)

filterPrimes :: [Int] -> [Int]
filterPrimes []     = []
filterPrimes (n:ns) = n : (filterPrimes (reject (\x -> x `mod` n == 0) ns))


-- Based on a solution provided by vineet.  MUCH faster than my seive method.
-- I like it and I need to embrace deeper recursive constructs like this.
isPrime :: Int -> Bool
isPrime n = ipp n primes
  where ipp n (p:ps)
          | p*p > n      = True
          | mod n p == 0 = False
          | otherwise    = ipp n ps

primes = 2 : (filter isPrime [3..])

main = do
  putStrLn $ show (primes !! 10001)
