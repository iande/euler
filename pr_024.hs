{--
A permutation is an ordered arrangement of objects. For example, 3124 is one
possible permutation of the digits 1, 2, 3 and 4. If all of the permutations
are listed numerically or alphabetically, we call it lexicographic order. The
lexicographic permutations of 0, 1 and 2 are:

012   021   102   120   201   210

What is the millionth lexicographic permutation of the digits
0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
--}

-- naive: (sort (permutations "0123456789")) !! 999999
-- Not really viable.  Let's start with some analysis.

fact :: Integer -> Integer
fact n = go n 1
  where go 0 a = a
        go 1 a = a
        go n a = go (n-1) (n*a)

numPerms :: [a] -> Integer
numPerms xs = fact ((fromIntegral . length) xs)

{--
Total of 3628800 permutations.

9! permutations begin with any given digit. => 362880

The resuls are in lexicographic order, so the we'll have 01(...) -> 09(...) as
the first 362880 elements, 10(...) -> 19(...) as the next 362880 elements
and so forth.
--}

-- Assumes list is pre-sorted in lexicographic order and the elements are distinct.
findLexPerm :: (Eq a) => [a] -> Integer -> [a]
findLexPerm [] _ = []
findLexPerm [x] _ = [x]
findLexPerm xs n = x' : (findLexPerm xs' n')
  where sp = fact ((fromIntegral . length) xs - 1)
        (i,n') = (divMod n sp)
        x' = xs !! (fromIntegral i)
        xs' = filter (/= x') xs

main = do
  print $ findLexPerm "0123456789" 999999
