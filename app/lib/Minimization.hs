module Minimization where

import FiniteAutomata
import Utils

minimize :: DFA -> DFA
minimize (DFA states alphabet start accepts (DFADelta transitions)) = DFA newStates alphabet newStart newAccepts newDelta
  where
    equivalenceSets = splitSet alphabet (DFADelta transitions) [filter (`notElem` accepts) states, accepts]
    newStates = map mergeAll equivalenceSets
    newStart = mergeAll $ head $ filter (start `elem`) equivalenceSets
    newAccepts = map mergeAll $ filter (any (`elem` accepts)) equivalenceSets
    getClass s = head $ filter (s `elem`) equivalenceSets
    newDelta = DFADelta $ removeDuplicates $ map (\(f, s, t) -> (mergeAll (getClass f), s, mergeAll (getClass t))) transitions

splitSet :: [Label] -> DFADelta -> [[StateSet]] -> [[StateSet]]
splitSet alphabet delta classes = if nextOrdered == classes then nextOrdered else splitSet alphabet delta nextOrdered
  where
    nextPartition = splitLabelbyLabel alphabet delta classes
    nextOrdered = sort $ map sort nextPartition

splitLabelbyLabel :: [Label] -> DFADelta -> [[StateSet]] -> [[StateSet]]
splitLabelbyLabel [] _ classes = classes
splitLabelbyLabel (x : xs) delta classes = splitLabelbyLabel xs delta nextSplit
  where
    nextSplit = splitByLabel x delta classes

splitByLabel :: Label -> DFADelta -> [[StateSet]] -> [[StateSet]]
splitByLabel label delta classes = concatMap (splitSetByLabel label delta classes) classes

splitSetByLabel :: Label -> DFADelta -> [[StateSet]] -> [StateSet] -> [[StateSet]]
splitSetByLabel label delta classes set = filter (not . null) $ map (\c -> map fst $ filter (\(_, t) -> t `elem` c) results) classes
  where
    results = map (\s -> (s, resolveTransitionDfa delta s label)) set
