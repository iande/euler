{--
2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
--}

-- lcm(a_1, lcm(a_2, lcm(a_3, ... lcm(a_{n-1}, a_n))))

lcmAll :: (Integral a) => [a] -> a
lcmAll = foldr lcm 1

main = do
  putStrLn (show (lcmAll [1..20]))
