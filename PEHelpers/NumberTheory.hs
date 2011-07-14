module PEHelpers.NumberTheory
(
  primes,
  divisorsIn,
  allDivisors,
  nonTrivialDivisors,
  firstPrimeDivisor,
  primeDivisors,
  primeFactorization,
  sumOfDivisors,
  aliquotSum,
  aliquotSequence,
  properDivisorsOf,
  divisorsOf,
  isAmicable,
  fibonacciNumbers
) where
  
import Data.List (group, sort, unfoldr)

-- Based on a solution provided by vineet.  MUCH faster than my seive method.
-- I like it and I need to embrace deeper recursive constructs like this.

-- (d:ds) must be in ascending order for this to work
isPrime [] n = True
isPrime (d:ds) n
  | d*d > n      = True
  | mod n d == 0 = False
  | otherwise    = isPrime ds n

primes = 2 : (filter (isPrime primes) [3..])

fsqrt :: Int -> Int
fsqrt n = floor . sqrt $ fromIntegral n

divisorsIn :: Int -> [Int] -> [(Int, Int)]
divisorsIn n xs = toPair . selDiv $ xs
            where toPair = map (\d -> (d, (div n d)))
                  selDiv = filter (\d -> mod n d == 0)

divisorsOf k = foldl accum [1,k] [2..(fsqrt k)]
  where accum ds n
          | m == 0    = if n == d then n : ds else n : d : ds
          | otherwise = ds
          where (d,m) = divMod k n

properDivisorsOf k = init $ divisorsOf k

allDivisors :: Int -> [(Int, Int)]
allDivisors n = divisorsIn n [1..(fsqrt n)]
                  
nonTrivialDivisors :: Int -> [(Int, Int)]
nonTrivialDivisors n = divisorsIn n [2..(fsqrt n)]

{--
  If no non-trivial divisors exist, the number is prime.
  Otherwise, the first non-trivial divisor is of the form (p,k) where p is
  a prime.  We then take the first non-trivial divisor of k, and so on
--}

firstPrimeDivisor :: Int -> (Int,Int)
firstPrimeDivisor n
  | null firstDiv = (n, 1)
  | otherwise     = head $ firstDiv
  where firstDiv = take 1 (nonTrivialDivisors n)

primeDivisors :: Int -> [Int]
primeDivisors n = takePrimes (firstPrimeDivisor n)
  where takePrimes (p,1) = [p]
        takePrimes (p,k) = p : (takePrimes (firstPrimeDivisor k))

-- As prime divisors are taken in order, we are free to group without sorting.
primeFactorization :: Int -> [(Int,Int)]
primeFactorization n = map (\g -> (head g, length g)) $ group (primeDivisors n)

sumOfDivisors k = sum . (map (\a -> a ^ k)) . divisorsOf

aliquotSum n = sumOfDivisors 1 n - n

aliquotSequence = iterate aliquotSum

isAmicable k = let ak = aliquotSum k in k /= ak && aliquotSum ak == k

fibonacciNumbers = unfoldr (\(f1,f2) -> Just (f1, (f2, f1 + f2))) (0, 1)
