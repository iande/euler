{--
A palindromic number reads the same both ways. The largest palindrome made
from the product of two 2-digit numbers is 9009 = 91  99.

Find the largest palindrome made from the product of two 3-digit numbers.
--}

cartesianProduct :: [a] -> [b] -> [(a,b)]
cartesianProduct xs ys = [ (x,y) | x <- xs, y <- ys ]

cartesianMap :: (a -> b -> c) -> [a] -> [b] -> [c]
cartesianMap f xs ys = map (\(x,y) -> f x y) $ cartesianProduct xs ys

isPalindrome :: String -> Bool
isPalindrome str = str == reverse str

isPalindromic :: Int -> Bool
isPalindromic n = isPalindrome (show n)

cartesianMult :: [Int] -> [Int] -> [Int]
cartesianMult = cartesianMap (*)

{--productsOfDigits k = map (\(a,b) -> a * b) $ cartesianProduct ns cs
  where l = 10 ^ (k - 1)
        u = 10 ^ k - 1
        ns = [(10 ^ (k - 1))..(10 ^ k - 1)]
        cs = [ (x,y) | x <- ns, y <- ns, x `mod` 11 == 0 ]
--}

numbersHavingDigits :: Int -> [Int]
numbersHavingDigits k = [(10 ^ (k - 1))..(10 ^ k - 1)]

initialCandidates = cartesianMult (filter (\x -> x `mod` 11 == 0) $ numbersHavingDigits 3) (numbersHavingDigits 3)

main = do
  putStrLn $ show (maximum $ filter isPalindromic initialCandidates)