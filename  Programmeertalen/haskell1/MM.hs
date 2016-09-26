-- Programmeertalen Haskell 1
-- Steven Raaijmakers, 10804242
-- Program simulates a Mastermind Solver, and shows the tries.

module MM

where
    -- Defining:
    import Data.List
    import Control.Monad (replicateM)

    data Color = Red | Yellow | Blue | Green | Orange | Purple
            deriving (Eq, Show, Bounded, Enum)

    type Pattern = [Color]

    type Feedback = (Int, Int)

    -- Counts (occurences from pattern 1 Colors in pattern 2) and (equal positions + colors)
    reaction :: Pattern -> Pattern -> Feedback
    reaction xs ys = (b, w - b)
           where b = (reaction' xs ys)
                 w = (reaction'' xs ys 0)
    reaction' :: Pattern -> Pattern -> Int
    reaction' xs ys = foldr(\x y -> if x then y + 1 else y) 0 xsys
         where xsys = zipWith (==) xs ys
    reaction'' :: Pattern -> Pattern -> Int -> Int
    reaction'' [] _ n = n
    reaction'' (x:xs) y n | elem x y = reaction'' xs (delete x y) (n + 1)
                          | otherwise = reaction'' xs y n

    -- Makes a list of all possible answers
    colors :: [Color]
    colors = [minBound..maxBound]
    store = replicateM 4 colors

    -- Breaks code, starting with head of store.
    -- Reduces store by removing items with different outcomes for reaction, compared to secret
    naive_algorithm :: Pattern -> [Pattern]
    naive_algorithm s = naive_algorithm' s store (0,0) []
    -- Helper function, to simplify input for user
    naive_algorithm' :: Pattern -> [Pattern] -> Feedback -> [Pattern] -> [Pattern]
    naive_algorithm' _ _ (4,0) ys = ys
    naive_algorithm' s xs a ys = naive_algorithm' s (filter' s x xs) (reaction s x) (x:ys)
                       where x = head xs
    -- Filter based on outcome for Reaction
    filter' :: Pattern -> Pattern -> [Pattern] -> [Pattern]
    filter' s x xs = filter(\y -> reaction y x == reaction s x) xs

    -- Counts tries for each element in store
    tester :: (Pattern -> [Pattern]) -> Double
    tester f = fromIntegral(tester' f store 0) / fromIntegral(length store)
    -- Helper function to simplify input for user
    tester' :: (Pattern -> [Pattern]) -> [Pattern] -> Int -> Int
    tester' _ [] n = n
    tester' f (x:xs) n = tester' f xs $ n + length (f x)
