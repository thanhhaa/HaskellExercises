import Distribution.Simple.Utils (xargs)
-- List Exercises

-- 1. Create a function elem that returns True if an element is in a given list
-- and returns False otherwise

elemN :: (Eq a) => a -> [a] -> Bool
elemN _ []     = False
elemN a (x:xs)
   | a == x    = True
   | otherwise = elemN a xs

elemNN :: (Eq a) => a -> [a] -> Bool
elemNN _ []     = False
elemNN a (x:xs) = (a == x) || elem a xs

-- 2. Create a function nub that removes all duplicates from a given list
nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (x:xs)
   | elemN x xs = nub xs
   | otherwise  = x : nub xs

-- 3. Create a function isAsc that returns True if the list given to it is
-- a list of ascending order
isAsc :: [Int] -> Bool
isAsc [] = False
isAsc (x:xs) = isAsc' x xs

isAsc' :: Int -> [Int] -> Bool
isAsc' _ [] = False
isAsc' a (x:xs)
   | a <= x && null xs  = True
   | a <= x             = isAsc' x xs
   | otherwise         = False

-- other
isAscNew :: [Int] -> Bool
isAscNew []       = True
isAscNew [x]      = True
isAscNew (x:y:xs) = (x < y) && isAscNew (y:xs)


-- 4. Create a function hasPath that determines if a path from one node to another
-- exists within a directed graph
hasPath :: [(Int, Int)] -> Int -> Int -> Bool
hasPath _ _ _ = False