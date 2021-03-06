{--
The sequence of triangle numbers is generated by adding the natural numbers.
So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. The first
ten terms would be:

1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

Let us list the factors of the first seven triangle numbers:

 1: 1
 3: 1,3
 6: 1,2,3,6
10: 1,2,5,10
15: 1,3,5,15
21: 1,3,7,21
28: 1,2,4,7,14,28
We can see that 28 is the first triangle number to have over five divisors.

What is the value of the first triangle number to have over five hundred
divisors?
--}

import PEHelpers.NumberTheory
import Data.List (find)

-- Clearly, the nth triangle number => n * (n + 1) / 2

triangleNumbers = map (\n -> n * (n + 1) `div` 2) [1..]
triangleDivisors = map allDivisors triangleNumbers

main = do
  -- allDivisors returns pairs of divisors, so we only need > 250
  -- The result will be the second element of the first tuple
  print $ find (\ds -> length ds > 250) triangleDivisors
