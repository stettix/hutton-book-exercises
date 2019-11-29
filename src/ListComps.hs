module ListComps where

s = sum [n ^ 2 | n <- [1 .. 100]]

grid m n = [(x, y) | x <- [0 .. m], y <- [0 .. n]]

square n = [(x, y) | (x, y) <- grid n n, x /= y]

replicate n x = [x | _ <- [1 .. n]]
