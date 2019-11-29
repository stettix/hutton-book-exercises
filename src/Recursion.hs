module Recursion where

factorial :: Int -> Int
factorial n
  | n < 1 = 1
factorial n = n * factorial (n - 1)

sumdown :: Int -> Int
sumdown n
  | n < 1 = 0
sumdown n = n + sumdown (n - 1)

(^) :: Int -> Int -> Int
(^) m 0 = 1
(^) m n = m * (Recursion.^) m (n - 1)

euclid :: Int -> Int -> Int
euclid m n
  | m == n = m
euclid m n
  | m < n = euclid m (n - m)
euclid m n
  | m > n = euclid (m - n) n

myand :: [Bool] -> Bool
myand []     = True
myand (x:xs) = x && myand xs

myconcat :: [[a]] -> [a]
myconcat []       = []
myconcat (xs:xss) = xs ++ myconcat xss

(!!!) :: [a] -> Int -> a
(!!!) (x:_) 0  = x
(!!!) (_:xs) n = (!!!) xs (n - 1)

myelem :: Eq a => [a] -> a -> Bool
myelem [] _ = False
myelem (x:_) y
  | x == y = True
myelem (_:xs) y = myelem xs y

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y : ys)
merge (x:xs) (y:ys)
  | x > y = y : merge (x : xs) ys

halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
  where
    n = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort xs') (msort xs'')
  where
    (xs', xs'') = halve xs
