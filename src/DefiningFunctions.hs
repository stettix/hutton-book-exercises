module DefiningFunctions where

qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

n = a `div` length xs
  where
    a = 10
    xs = [1, 2, 3, 4, 5]

mylast xs = drop (length xs - 1) xs

mylast2 xs = reverse (take 1 (reverse xs))

mylast3 xs = [xs !! (length xs - 1)]

mylast4 xs = head (reverse xs)

myinit xs = take (length xs - 1) xs

myinit2 xs = reverse (drop (length xs - 1) (reverse xs))

halve xs = (take n xs, drop n xs)
  where
    n = length xs `div` 2

third1 xs = head (tail (tail xs))

third2 xs = xs !! 2

third3 (_:_:x:_) = x

safetail1 xs =
  if null xs
    then []
    else tail xs

safetail2 xs
  | null xs = []
safetail2 xs
  | otherwise = tail xs

safetail3 []     = []
safetail3 (x:ys) = ys

(||) :: Bool -> Bool -> Bool
_ || True = True
_ || False = False

--(||) :: Bool -> Bool -> Bool
--True || _ = True
--False || _ = False
-- etc...

mult :: Int -> Int -> Int -> Int
mult = \a -> \b -> \c -> a * b * c

luhnDouble x =
  if doubleX > 9
    then doubleX - 9
    else doubleX
  where
    doubleX = x * 2

luhn4 a b c d = (luhnDouble a + b + luhnDouble c + d) `mod` 10 == 0

