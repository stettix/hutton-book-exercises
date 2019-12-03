module Hof where

listComp :: (a -> b) -> (a -> Bool) -> [a] -> [b]
listComp f p xs = map f (filter p xs)

myall :: (a -> Bool) -> [a] -> Bool
myall p xs = foldl (&&) True (map p xs)

myany :: (a -> Bool) -> [a] -> Bool
myany p xs = foldl (||) False (map p xs)

mytakeWhile :: (a -> Bool) -> [a] -> [a]
mytakeWhile _ [] = []
mytakeWhile p (x : xs) | p x = x : mytakeWhile p xs
                       | otherwise = []

mydropWhile :: (a -> Bool) -> [a] -> [a]
mydropWhile _ [] = []
mydropWhile p (x : xs) | p x = mydropWhile p xs
                       | otherwise = x : xs

mymap :: (a -> b) -> [a] -> [b]
mymap f = foldr (\x acc -> f x : acc) []

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter p = foldr (\x acc -> if p x then x : acc else acc) []

dec2int :: [Int] -> Int
dec2int = foldl (\acc x -> acc * 10 + x) 0

mycurry :: ((a, b) -> c) -> a -> b -> c
mycurry f x y = f (x, y)

myuncurry :: (a -> b -> c) -> (a, b) -> c
myuncurry f (x, y) = f x y

unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

int2bin = unfold (== 0) (`mod` 2) (`div` 2)

type Bit = Int

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8)

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g (x : x' : xs) = f x : g x' : altMap f g xs
altMap _ _ [] = []
altMap f _ [x] = [f x]

luhnDouble x =
  if doubleX > 9
    then doubleX - 9
    else doubleX
  where
    doubleX = x * 2

luhn xs = sum (altMap luhnDouble id xs) `mod` 10 == 0
