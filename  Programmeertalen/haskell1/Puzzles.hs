-- Programmeertalen Haskell 1
-- Steven Raaijmakers, 10804242
-- Program simulates a few standard functions

module Puzzles

where
    -- 1
    length' xs = foldr(\_ n -> (n + 1)) 0 xs

    -- 2
    elem' x xs = foldr((\x x' xs -> (x == x' || xs))x) False xs

    -- 3
    or' xs = foldr(\x xs -> (x == True || xs)) False xs

    -- 4
    map' f xs = foldr(\x y -> f x:y) [] xs

    -- 5
    plusplus xs ys = foldr(\x y -> x:y) ys xs

    -- 6
    reverse' xs = foldr(\x y -> plusplus y [x]) [] xs

    -- 7
    reverse'' xs = foldl (\x y -> y:x) [] xs

    -- 8
    get' n xs = snd $ foldl (\x (i, j) -> if i == n then (i, j) else x) (0, 0) (zip [1,2..] xs)

    -- 9
    isPalindrome :: String -> Bool
    isPalindrome x = x == reverse x

    -- 10
    fib = 1 : scanl (+) 1 fib
