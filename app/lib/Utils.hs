module Utils where

join :: (Show a) => String -> [a] -> String
join sep = foldr (\x str -> show x ++ if str == "" then str else sep ++ str) ""

joinStrings :: String -> [String] -> String
joinStrings sep = foldr (\x str -> x ++ if str == "" then str else sep ++ str) ""

splitString :: (Eq a) => [a] -> [a] -> [[a]]
splitString separator = splitHelper
  where
    splitHelper [] = [[]]
    splitHelper xs
      | take (length separator) xs == separator = [] : splitHelper (drop (length separator) xs)
      | otherwise = (head xs : head rest) : tail rest
      where
        rest = splitHelper (tail xs)

split :: (Eq a) => [a] -> [a] -> [[a]]
split _ [] = [[]]
split sep xs
  | take (length sep) xs == sep = [] : split sep (drop (length sep) xs)
  | otherwise = (head xs : head rest) : tail rest
  where
    rest = split sep (tail xs)

powerSet :: [a] -> [[a]]
powerSet [] = [[]]
powerSet (x : xs) = map (x :) sub ++ sub
  where
    sub = powerSet xs

sort :: (Ord a) => [a] -> [a]
sort [] = []
sort (x : xs) = sort less ++ [x] ++ sort greater
  where
    less = filter (< x) xs
    greater = filter (>= x) xs

removeDuplicates :: (Eq a, Ord a) => [a] -> [a]
removeDuplicates l = r $ sort l
  where
    r [] = []
    r [x] = [x]
    r (x : y : xs) = if x == y then r (y : xs) else x : r (y : xs)
