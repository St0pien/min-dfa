module Utils where

join :: (Show a) => String -> [a] -> String
join sep = foldr (\x str -> show x ++ if str == "" then str else sep ++ str) ""

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

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x : xs) = if x `elem` xs then removeDuplicates xs else x : removeDuplicates xs
