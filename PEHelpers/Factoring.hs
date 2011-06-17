module PEHelpers.Factoring
(
  divisorsIn,
  allDivisors,
  nonTrivialDivisors,
  firstPrimeDivisor,
  primeDivisors,
  primeFactorization
) where
  
import Data.List (group)

fsqrt :: Double -> Int
fsqrt = floor . sqrt

divisorsIn :: Int -> [Int] -> [(Int, Int)]
divisorsIn n xs = toPair . selDiv $ xs
            where toPair = map (\d -> (d, (div n d)))
                  selDiv = filter (\d -> mod n d == 0)

allDivisors :: Int -> [(Int, Int)]
allDivisors n = divisorsIn n [1..(fsqrt (fromIntegral n))]
                  
nonTrivialDivisors :: Int -> [(Int, Int)]
nonTrivialDivisors n = divisorsIn n [2..(fsqrt (fromIntegral n))]

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

