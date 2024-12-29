module Determinisation where

import FiniteAutomata
import UnachieveableRemoval (removeUnachieveableDfa)
import Utils

determinise :: NFA -> DFA
determinise (NFA states alphabet start accepts delta) = removeUnachieveableDfa fullDfa
  where
    newStates = statesPowerSet states
    fullDfa = DFA newStates alphabet start (findAcceptStates accepts newStates) (remapDelta delta newStates alphabet)

statesPowerSet :: [State] -> [State]
statesPowerSet states = map mergeAll (powerSet states)

findAcceptStates :: [State] -> [State] -> [State]
findAcceptStates old = filter (\s -> any (`belongs` s) old)

remapDelta :: NFADelta -> [State] -> [Label] -> DFADelta
remapDelta delta states alphabet = DFADelta $ [(from, label, pickToSet delta from label) | from <- states, label <- alphabet]

pickToSet :: NFADelta -> State -> Label -> State
pickToSet delta (StateSet fromStates) label = mergeAll $ concatMap (\s -> resolveTransitionNfa delta (State s) label) fromStates
pickToSet delta (State from) label = mergeAll $ resolveTransitionNfa delta (State from) label
