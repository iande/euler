{--
Starting in the top left corner of a 22 grid, there are 6 routes (without
backtracking) to the bottom right corner.

How many routes are there through a 2020 grid?
--}
{--

Imagine the grid has numbers in it.  We know that for nx1 or 1xn we have n+1
paths available.  So, we can fill in the bottom row and the right column with
these values.  Here's a 4x4 demonstration (start numbering at bottom right)

|   |   |   | 5 |
|   |   |   | 4 |
|   |   |   | 3 |
| 5 | 4 | 3 | 2 |

Every blank cell is the sum of its right and bottom neighbors:

| 70 | 35 | 15 | 5 |
| 35 | 20 | 10 | 4 |
| 15 | 10 |  6 | 3 |
|  5 |  4 |  3 | 2 |

The result in the upper left corner is the number of possible paths available.

For the sake of the naked ape, I'm going to invert this matrix:

|  2 |  3 |  4 |  5 |
|  3 |  6 | 10 | 20 |
|  4 | 10 | 20 | 35 |
|  5 | 15 | 35 | 70 |

--}

-- Fire in the Taco Bell

-- We are doing more work than necessary, but this is fast enough
-- Also, joining lists with ++ bothers me, but they are of small sizes
nthRow k 1 = [2..(k+1)]
nthRow k n = foldl (\a i -> a ++ [i + (last a)]) [start] (tail prevRow)
  where prevRow = nthRow k (n-1)
        start   = n + 1

pathCount n = last (nthRow n n)


main = do
  print $ pathCount 20