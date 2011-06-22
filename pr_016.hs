{--
2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

What is the sum of the digits of the number 2^1000?
--}
-- This feels like cheating, since Haskell natively supports big ole integers.
-- Let's at least make our digit summer interesting...

sumDigits n radix = sdacc n 0
  where sdacc 0 s = s
        sdacc k s = let (d,m) = divMod k radix in sdacc d (s + m)

main = do
  print $ sumDigits (2 ^ 1000) 10
