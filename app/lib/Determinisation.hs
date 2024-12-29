module Determinisation(determinise) where

import FiniteAutomata
import UnachieveableRemoval (removeUnachieveableDfa)
import Utils

determinise :: NFA -> DFA
determinise (NFA states alphabet (State start) accepts delta) = removeUnachieveableDfa fullDfa
  where
    newStates = statesPowerSet states
    fullDfa = DFA newStates alphabet (StateSet [start]) (findAcceptStates accepts newStates) (remapDelta delta newStates alphabet)

statesPowerSet :: [State] -> [StateSet]
statesPowerSet states = map (StateSet . map unpack . sort) $ powerSet states
  where
    unpack (State s) = s

findAcceptStates :: [State] -> [StateSet] -> [StateSet]
findAcceptStates old = filter (\s -> any (`belongs` s) old)

remapDelta :: NFADelta -> [StateSet] -> [Label] -> DFADelta
remapDelta delta states alphabet = DFADelta $ [(from, label, pickToSet delta from label) | from <- states, label <- alphabet]

pickToSet :: NFADelta -> StateSet -> Label -> StateSet
pickToSet delta (StateSet fromStates) label = mergeAll $ map (\s -> resolveTransitionNfa delta (State s) label) fromStates
