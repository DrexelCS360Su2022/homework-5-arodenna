{-# OPTIONS_GHC -fwarn-tabs #-}

module HaskellIntro where

import Set

-- Load this file into GHCi (say, with `ghci HaskellIntro.hs`) and type
-- `isThisWorking` at the prompt. GHCi will tell you whether it's working!

isThisWorking :: String
isThisWorking = "Finally"

--
-- Problem 1
--

-- 1.1
lastDigit :: Integer -> Integer
lastDigit x = x `rem` 10

dropLastDigit :: Integer -> Integer
dropLastDigit x = x `quot` 10

-- 1.2
toDigits :: Integer -> [Integer]
toDigits x 
    | x <= 0 = []
    | otherwise = toDigits (dropLastDigit x) ++ [lastDigit x]

-- 1.3
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = listReverser (deoHelper (listReverser x))


deoHelper :: [Integer] -> [Integer]
deoHelper x 
    | x == [] = []
    | length x == 1 = x
    | length x == 2 = [(head x)] ++ [(2*(head (tail x)))]
    | otherwise = [(head x)] ++ [(2*(head (tail x)))] ++ deoHelper (tail (tail x))

listReverser :: [Integer] -> [Integer]
listReverser myList
    | myList == [] = []
    | otherwise = listReverser (tail myList) ++ [(head myList)]

-- 1.4
sumDigits :: [Integer] -> Integer
sumDigits myList 
    | myList == [] = 0
    | (head myList) >= 10 = (sumDigits (toDigits (head myList))) + (sumDigits (tail myList))
    | otherwise =  (head myList) + (sumDigits (tail myList))

-- 1.5
validate :: Integer -> Bool
validate myList 
    | sumDigits(doubleEveryOther(toDigits(myList))) `rem` 10 == 0 = True
    | otherwise = False

--
-- Problem 2
--

-- 2.1
pow :: (a -> a) -> Int -> a -> a
pow f n x
    | n == 0 = x
    | otherwise = pow f (n - 1) (f x)

-- 2.2
g :: Integer -> Integer
g x 
    | x == 0 = 0
    | otherwise = x - (pow g 2 (x - 1))

h :: Integer -> Integer
h x 
    | x == 0 = 0
    | otherwise = x - (pow h 3 (x - 1))

-- 2.3
d :: Int -> Integer -> Integer
d i x 
    | x == 0 = 0 
    | otherwise = x - (pow (d i) i (x-1))

--
-- Problem 3
--

powerSet :: Ord a => Set a -> Set(Set a)
powerSet mySet 
    | isEmpty mySet = insert mySet empty --If mySet is empty, add empty set
    | otherwise = let (myHead, myTail) = split mySet in (powerSet myTail) `union` (mapSet (insert myHead) (powerSet myTail))
-- Otherwise, split the set into head and tail and union the following
    -- the result of calling powerset on the rest/tail of the set
    -- all combinations of the head of the set with the tail of the set
            -- Head + Tail (size n); Head + Each Combination of Tail - 1 Element (size n-1); ... Head (size n - (n-1))
            -- aka append head to the powerset of the tail
