{--
By starting at the top of the triangle below and moving to adjacent numbers
on the row below, the maximum total from top to bottom is 23.

3
7 4
2 4 6
8 5 9 3

That is, 3 + 7 + 4 + 9 = 23.

Find the maximum total from top to bottom of the triangle below:

75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23

NOTE: As there are only 16384 routes, it is possible to solve this problem by
trying every route. However, Problem 67, is the same challenge with a
triangle containing one-hundred rows; it cannot be solved by brute force,
and requires a clever method! ;o)
--}

{--
This is pretty straightfoward, we just start at the bottom and fold up.
Consider the 3rd element from the second to last row (04).  From 04 we can
go to 98 or 27.  Let M(04) be the maximal path length up to this element.
Our final path length is then one of: M(04) + 98 or M(04) + 27.  Clearly,
M(04) + 98 > M(04) + 27, so from 04 we would choose 98.  Taking the last
two rows:

63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23

we would fold up these two into one as:

(63+62) (66+98) (4+98) (68+27) (89+23) (53+70) (67+98) (30+98) (73+93)
  (16+93) (69+53) (87+60) (40+60) (31+23)

Then repeat the procedure until you end up with the maximum path weight
--}

triangle = ["75",
            "95 64",
            "17 47 82",
            "18 35 87 10",
            "20 04 82 47 65",
            "19 01 23 75 03 34",
            "88 02 77 73 07 63 67",
            "99 65 04 28 06 16 70 92",
            "41 41 26 56 83 40 80 70 33",
            "41 48 72 33 47 32 37 16 94 29",
            "53 71 44 65 25 43 91 52 97 51 14",
            "70 11 33 28 77 73 17 78 39 68 17 57",
            "91 71 52 38 17 14 91 43 58 50 27 29 48",
            "63 66 04 68 89 53 67 30 73 16 69 87 40 31",
            "04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"]

numTriangle = map (\ns -> map (\n -> (read n)::Int) ns) (map words triangle)

maxPairing ns = zipWith max (init ns) (tail ns)

foldUp ns nxs = map (\(n,m) -> n + m) $ zip ns (maxPairing nxs)

foldEmAll ns = foldr foldUp (last ns) (init ns)

main = do
  print $ foldEmAll numTriangle
