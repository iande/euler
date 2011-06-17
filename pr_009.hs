{--
A Pythagorean triplet is a set of three natural numbers, a  b  c, for which,

a2 + b2 = c2
For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
--}

candidates = [ (a,b,(1000 - a - b)) | a <- [1..332], b <- [(a+1)..((1000 - a) `div` 2)] ]
pythagTrips = filter (\(a,b,c) -> a^2 + b^2 == c^2) candidates
prod = map (\(a,b,c) -> a * b * c) pythagTrips

main = do
  putStrLn $ show prod
