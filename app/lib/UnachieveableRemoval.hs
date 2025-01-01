module UnachieveableRemoval(removeUnachieveableDfa, removeUnachieveableNfa) where

import FiniteAutomata
import Utils

removeUnachieveableDfa :: DFA -> DFA
removeUnachieveableDfa (DFA _ _ start accepts (DFADelta transitions)) = DFA achieveable newAlphabet start newAccepts (DFADelta cleanedDelta)
  where
    achieveable = findAchieveableFrom getNeighborsDfa transitions [] start
    cleanedDelta = filter (\(f, _, t) -> f `elem` achieveable && t `elem` achieveable) transitions
    newAlphabet = correspondingAlphabet cleanedDelta
    newAccepts = filter (`elem` achieveable) accepts

removeUnachieveableNfa :: NFA -> NFA
removeUnachieveableNfa (NFA _ _ start accepts (NFADelta transitions)) = NFA achieveable newAlphabet start newAccepts (NFADelta cleanedDelta)
  where
    achieveable = findAchieveableFrom getNeighborsNfa transitions [] start
    cleanedDelta = filter (\(f, _, StateSet t) -> f `elem` achieveable && not (null t)) $ map (\(f, s, StateSet t) -> (f, s, StateSet (filter (\tt -> State tt `elem` achieveable) t))) transitions
    newAlphabet = correspondingAlphabet cleanedDelta
    newAccepts = filter (`elem` achieveable) accepts

findAchieveableFrom :: (Eq a, Ord a) => ([(a, Label, b)] -> a -> [a]) -> [(a, Label, b)] -> [a] -> a -> [a]
findAchieveableFrom getNeighbors transitions result from = newResult ++ filter (`notElem` newResult) achieveable
  where
    newResult = from : result
    neighbors = filter (`notElem` newResult) $ getNeighbors transitions from
    achieveable = removeDuplicates $ concatMap (findAchieveableFrom getNeighbors transitions newResult) neighbors

correspondingAlphabet :: [(a, Label, b)] -> [Label]
correspondingAlphabet transitions = filter (/= Eps) $ removeDuplicates $ map (\(_, s, _) -> s) transitions
