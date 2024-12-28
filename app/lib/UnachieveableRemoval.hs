module UnachieveableRemoval (removeUnachieveable) where

import FiniteAutomata
import Utils

removeUnachieveable :: NFA -> NFA
removeUnachieveable (NFA states alphabet start accepts delta) = NFA achievable (correspondingAlphabet cleanedDelta) start accepts cleanedDelta
  where
    achievable = findAchieveable (NFA states alphabet start accepts delta)
    cleanedDelta = correspondingTransitions achievable delta

findAchieveable :: NFA -> [State]
findAchieveable (NFA _ _ start _ delta) = findAchieveableFrom delta [] start

findAchieveableFrom :: NFADelta -> [State] -> State -> [State]
findAchieveableFrom delta result from = newResult ++ filter (`notElem` newResult) achievable
  where
    newResult = from : result
    neighbors = filter (`notElem` newResult) $ getNeighbors delta from
    achievable = removeDuplicates $ concatMap (findAchieveableFrom delta newResult) neighbors

getNeighbors :: NFADelta -> State -> [State]
getNeighbors (NFADelta transitions) from = concatMap (\(_, _, t) -> t) $ filter (\(f, _, _) -> f == from) transitions

correspondingTransitions :: [State] -> NFADelta -> NFADelta
correspondingTransitions states (NFADelta transitions) =
  NFADelta $
    filter (\(f, _, t) -> f `elem` states && not (null t)) $
      map (\(f, s, t) -> (f, s, filter (`elem` states) t)) transitions

correspondingAlphabet :: NFADelta -> [Label]
correspondingAlphabet (NFADelta transitions) = filter (/= Eps) $ removeDuplicates $ map (\(_, s, _) -> s) transitions
