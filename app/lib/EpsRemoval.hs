module EpsRemoval (removeEpsTransitions) where

import FiniteAutomata
import Utils (removeDuplicates)

removeEpsTransitions :: NFA -> NFA
removeEpsTransitions (NFA states symbols start accepts delta) = NFA states symbols start extendedAccepts newDelta
  where
    allPossibleTransitions = [(x, y) | x <- states, y <- symbols]
    newDelta = NFADelta $ filter (\(_, _, StateSet t) -> not (null t)) $ map (\(f, s) -> (f, s, expandDelta states delta f s)) allPossibleTransitions
    extendedAccepts = filter (\s -> any (`elem` epsClosure delta s) accepts) states

epsClosure :: NFADelta -> State -> [State]
epsClosure delta = epsClosure' delta []

epsClosure' :: NFADelta -> [State] -> State -> [State]
epsClosure' delta result from = newResult ++ filter (`notElem` newResult) achievable
  where
    newResult = from : result
    neighbors = filter (`notElem` newResult) $ getNeighbors delta from
    achievable = removeDuplicates $ concatMap (epsClosure' delta newResult) neighbors

getNeighbors :: NFADelta -> State -> [State]
getNeighbors (NFADelta transitions) from = map State $ concatMap (\(_, _, StateSet t) -> t) $ filter (\(f, s, _) -> f == from && s == Eps) transitions

expandDelta :: [State] -> NFADelta -> State -> Label -> StateSet
expandDelta states delta from label = StateSet $ map (\(State t) -> t) $ concatMap (epsClosure delta) possibleEnds
  where
    possibleStarts = epsClosure delta from
    possibleEnds = filter (\t -> any (\f -> transitionExists delta f label t) possibleStarts) states

transitionExists :: NFADelta -> State -> Label -> State -> Bool
transitionExists (NFADelta transitions) from label to = any (\(f, s, t) -> f == from && label == s && to `belongs` t) transitions
