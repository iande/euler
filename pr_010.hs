{--
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.

2  = 2
5  = 2 + 3
10 = 2 + 3 + 5
17 = 2 + 3 + 5 + 7
28 = 2 + 3 + 5 + 7 + 11
41 = 2 + 3 + 5 + 7 + 11 + 13


--}

import PEHelpers.Primes



main = do
  print $ sum $ takeWhile (\n -> n < 2000000) primes
