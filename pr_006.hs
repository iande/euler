{--
The sum of the squares of the first ten natural numbers is,

1^2 + 2^2 + ... + 10^2 = 385
The square of the sum of the first ten natural numbers is,

(1 + 2 + ... + 10)^2 = 55^2 = 3025
Hence the difference between the sum of the squares of the first ten natural
numbers and the square of the sum is 3025 - 385 = 2640.

Find the difference between the sum of the squares of the first one hundred
natural numbers and the square of the sum.
--}

-- Square of the sum of the first n numbers => (n * (n+1) / 2)^2
-- So, we know that the square of the sums of the first 100 = (50 * 101 / 2)^2
-- => 6375625

-- Let's try a direct computation

sumOfSquares :: Int -> Int
sumOfSquares n = sum $ map (\x -> x^2) [1..n]

squareOfSums :: Int -> Int
squareOfSums n = (n * (n + 1) `div` 2) ^ 2

main = do
  putStrLn $ show (squareOfSums 100 - sumOfSquares 100)