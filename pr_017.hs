{--
If the numbers 1 to 5 are written out in words: one, two, three, four, five,
then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were written out
in words, how many letters would be used?

NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20
letters. The use of "and" when writing out numbers is in compliance with
British usage.
--}

-- First a note: Screw you Britania. We use "and" to denote a decimal point.
-- America, fuck yeah!

-- Second, a follow-up: Screw all of us English speakers for having such
-- stupid conventions for naming our numbers. 12 should be "one-ten two" not
-- "twelve".

spellItOut :: Int -> String
-- A bunch of special cases, because English number naming sucks.
spellItOut 1 = "one"
spellItOut 2 = "two"
spellItOut 3 = "three"
spellItOut 4 = "four"
spellItOut 5 = "five"
spellItOut 6 = "six"
spellItOut 7 = "seven"
spellItOut 8 = "eight"
spellItOut 9 = "nine"
spellItOut 10 = "ten"
spellItOut 11 = "eleven"
spellItOut 12 = "twelve"
spellItOut 13 = "thirteen"
spellItOut 14 = "fourteen"
spellItOut 15 = "fifteen"
spellItOut 16 = "sixteen"
spellItOut 17 = "seventeen"
spellItOut 18 = "eighteen"
spellItOut 19 = "nineteen"
spellItOut 20 = "twenty"
spellItOut 30 = "thirty"
spellItOut 40 = "forty"
spellItOut 50 = "fifty"
spellItOut 60 = "sixty"
spellItOut 70 = "seventy"
spellItOut 80 = "eighty"
spellItOut 90 = "ninety"

-- Skip the spaces, we aren't counting them, only the actual letters
spellItOut n
  | n == 1000 = "onethousand"
  | n >= 100 = spellItOut d100 ++ "hundred" ++ if m100 > 0 then "and" ++ spellItOut m100 else ""
  | otherwise = spellItOut (10 * d10) ++ spellItOut m10
  where (d100,m100) = divMod n 100
        (d10,m10)   = divMod n 10 

spelledOut = map spellItOut

main = do
  print $ sum (map length (spelledOut [1..1000]))
