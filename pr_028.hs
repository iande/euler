{--
Starting with the number 1 and moving to the right in a clockwise direction a
5 by 5 spiral is formed as follows:

21 22 23 24 25
20  7  8  9 10
19  6  1  2 11
18  5  4  3 12
17 16 15 14 13

It can be verified that the sum of the numbers on the diagonals is 101.

What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral
formed in the same way?
--}

{--
First thing to note: 
            21 - 25 = -4
             7 - 9 = -2
             5 - 3 =  2
            17 - 13 =  4

Also, along the diagonal running UR -> LL, 25 = 5^2, 9 = 3^2, 1 = 1^2, 5 = 2^2 + 1, 17 = 4^2 + 1

I think this solves it, but I should produce some kind of proof to prove
I'm right.
--}

-- n is the size of the square matrix, and must be odd.
sumOfSpiral n = n * n + gaus - 1 + 2 * (sum sqrs) + (div (n-1) 2)
  where sqrs = map (^2) [1..(n-1)]
        gaus = div (n * (n + 1)) 2

main = do
  print $ sumOfSprial 1001
  
{--
The answer of 669171001 checks out, but I would like to spend some more time
to verify the correctness of this expression.
--}