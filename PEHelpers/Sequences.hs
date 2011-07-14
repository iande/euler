module PEHelpers.Sequences
(
  primes,
  fibonaccis,
  tribonaccis
) where
  
import Data.List (unfoldr, lookup)
import qualified Data.Map as M

isPrime [] n = True
isPrime (d:ds) n
  | d*d > n      = True
  | mod n d == 0 = False
  | otherwise    = isPrime ds n

primes = 2 : (filter (isPrime primes) [3..])

genRecur f h s = let vg = h : (scanl f s vg) in vg
genRecur1 f h = let vg = (scanl f h vg) in vg

tribTuples = genRecur (\(a,b) (c,d) -> (b, b + c + d)) (0,0) (0,1)

fibonaccis = genRecur (+) 0 1
tribonaccis = map fst tribTuples

collatzSequence n = unfoldr (\k -> Just (k, collatz k)) n
  where collatz k
          | even k    = div k 2
          | otherwise = 3 * k + 1

-- Collatz sequence, minus the 4,2,1 cycle
principleCollatzSequence k = takeUntil (== 1) (collatzSequence k)

takeUntil _ [] = []
takeUntil p (n:ns)
  | p n       = [n]
  | otherwise = n : takeUntil p ns

collatz n
  | even n    = (div n 2)
  | otherwise = 3 * n + 1

insertSequence ms n = fst $ foldr (\k (a,l) -> (M.insert k l a, l+1)) (ms,1) (principleCollatzSequence n)
collatzUntil :: M.Map Int Int -> Int -> M.Map Int Int
collatzUntil ms 0 = ms
collatzUntil ms n = case (M.lookup n ms) of
  Just _  -> collatzUntil ms (n-1)
  Nothing -> collatzUntil (insertSequence ms n) (n-1)

collatzLengths = collatzUntil (M.fromList [(1,1)])

maxCollLen _ [] p = p
maxCollLen p2s (k:ks) (mn,ml)
  | kl > ml   = maxCollLen p2s rmks (kp,kl)
  | otherwise = maxCollLen p2s rmks (mn,ml)
  where kseq = principleCollatzSequence k
        (p2seq,p2l) = foldl (\(as,l) x -> let pk = k*x in if (elem pk ks) then (pk:as,l+1) else (as,l)) ([],0) p2s
        kl   = p2l + length kseq
        kp   = k * (2 ^ p2l)
        rmks = filter (\x -> (notElem x kseq) && (notElem x p2seq)) ks
        
clog2 :: Int -> Int
clog2 k = ceiling (log ((fromIntegral k)) / (log 2))

maxColletzLength n = maxCollLen [ 2 ^ x | x <- [0..(clog2 n)] ] [1..n] (0,0)
