{--
Euler published the remarkable quadratic formula:

n² + n + 41

It turns out that the formula will produce 40 primes for the consecutive 
values n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is
divisible by 41, and certainly when n = 41, 41² + 41 + 41 is clearly
divisible by 41.

Using computers, the incredible formula  n² - 79n + 1601 was discovered,
which produces 80 primes for the consecutive values n = 0 to 79.
The product of the coefficients, 79 and 1601, is 126479.

Considering quadratics of the form:

n² + an + b, where |a|  1000 and |b|  1000

where |n| is the modulus/absolute value of n
e.g. |11| = 11 and |4| = 4
Find the product of the coefficients, a and b, for the quadratic expression
that produces the maximum number of primes for consecutive values of n,
starting with n = 0.
--}

import PEHelpers.NumberTheory
import Data.List (sortBy)

{--
Suppose gcd(a,b) = k > 1.
Then: n^2 + an + b => n^2 + k(a'n + b')
We start with n = 0, so we immediately end up with a composite number.
Thus, gcd(a,b) = 1.

Further, mere relative primality is not enough.  For example, suppose
gcd(a,b) = 1, but b = k*m for some k > 1, |m| > 1.  Then, again when n = 0,
we have 0 + 0 + k*m => k*m, a composite.  So, in addition to a and b being
relatively prime, b must also be a true prime number.

Clearly, a /= 0, otherwise when n = 1, we end up with an even number (assuming
|b| /= 2, because that'd be a crappy choice for b anyway.) So, a > 0

More generally, a should not be even.  If it is, n = 0 might produce a prime,
but n = 1 will produce => 1 + 2k + p => 2k', an even number. Thus, a = 2k + 1
--}

concatNegated :: Num a => [a] -> [a]
concatNegated xs = (map negate xs) ++ xs

principlesForA = map ((+ 1) . (* 2)) [0..499]
-- skip 2.
principlesForB = tail $ takeWhile (< 1000) primes

candidatesForA = concatNegated principlesForA
candidatesForB = principlesForB

{--
This leaves us with 500 candidates for |a| and 168 candidates for |b|.  Thus,
1000 candidates for a and 336 candidates for b for a total of 336000 possible
combinations.  This might be doable.

Further refinement: If |b| > |a| and b < 0, then the first number will be < 0.
While not explicitly stated, I believe "primes" implies positive integers. So,
for n = 1, we need b > |(1 + a)| (when a < 0)  Further, when n = 0, we need
a positive number, so b > 0.
--}

candidateCoefficients = [ (a,b) | a <- candidatesForA, b <- candidatesForB ]

numberOfGeneratedPrimes :: (Integer, Integer) -> Integer
numberOfGeneratedPrimes coefs = go coefs 0
  where go (a,b) n
          | p < 0            = n
          | isPrime primes p = go (a,b) (n+1)
          | otherwise        = n
          where p = n*n + a*n + b


generatedPrimes = map (\c -> (c, numberOfGeneratedPrimes c)) candidateCoefficients

-- We know a = 1, b = 41 produces 40 primes, so we'll ignore anything less.
reducedPrimes = filter (\(_,n) -> n >= 40) generatedPrimes

-- A bit of sorting
sortedReduced = sortBy (\(_,n1) (_,n2) -> compare n2 n1) reducedPrimes

main = do
  putStrLn $ "a = " ++ (show cA) ++ ", b = " ++ (show cB) ++ " yields " ++ (show g) ++ " primes (" ++ (show pC) ++ ")"
  where ((cA, cB), g) = head sortedReduced
        pC = cA * cB
