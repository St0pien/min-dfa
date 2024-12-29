module FiniteAutomata where

import Utils

newtype State = State String

instance Show State where
  show (State str) = str

instance Eq State where
  (==) (State a) (State b) = a == b

instance Ord State where
  (<=) (State a) (State b) = a <= b

newtype StateSet = StateSet [String]

belongs :: State -> StateSet -> Bool
belongs (State a) (StateSet b) = a `elem` b

merge :: StateSet -> StateSet -> StateSet
merge (StateSet a) (StateSet b) = StateSet $ removeDuplicates (a ++ b)

mergeAll :: [StateSet] -> StateSet
mergeAll [] = StateSet []
mergeAll (x : xs) = foldl merge x xs

instance Show StateSet where
  show (StateSet []) = "empty"
  show (StateSet states) = joinStrings "" (sort states)

instance Eq StateSet where
  (==) (StateSet a) (StateSet b) = all (`elem` b) a && all (`elem` a) b

instance Ord StateSet where
  (<=) (StateSet a) (StateSet b) = a <= b

data Label = Eps | Label String

instance Show Label where
  show Eps = "_"
  show (Label str) = str

instance Eq Label where
  (==) (Label x) (Label y) = x == y
  (==) Eps Eps = True
  (==) _ _ = False

instance Ord Label where
  (<=) (Label x) (Label y) = x <= y
  (<=) Eps _ = True
  (<=) _ Eps = False

newtype NFADelta = NFADelta [(State, Label, StateSet)]

resolveTransitionNfa :: NFADelta -> State -> Label -> StateSet
resolveTransitionNfa (NFADelta transitions) from label = mergeAll $ map (\(_, _, t) -> t) $ filter (\(f, s, _) -> f == from && s == label) transitions

instance Show NFADelta where
  show (NFADelta transitions) =
    "delta = \n"
      ++ concatMap
        ( \(from, label, StateSet to) ->
            "\t" ++ show from ++ "," ++ show label ++ " -> " ++ join "," to ++ ";\n"
        )
        transitions

newtype DFADelta = DFADelta [(StateSet, Label, StateSet)]

resolveTransitionDfa :: DFADelta -> StateSet -> Label -> StateSet
resolveTransitionDfa (DFADelta transitions) from label = head . map (\(_, _, t) -> t) $ filter (\(f, s, _) -> f == from && s == label) transitions

instance Show DFADelta where
  show (DFADelta transitions) =
    "delta = \n"
      ++ concatMap
        ( \(from, label, to) ->
            "\t" ++ show from ++ "," ++ show label ++ " -> " ++ show to ++ ";\n"
        )
        transitions

data NFA = NFA [State] [Label] State [State] NFADelta

getNeighborsNfa :: [(State, Label, StateSet)] -> State -> [State]
getNeighborsNfa transitions from = map State result
  where
    (StateSet result) = mergeAll $ map (\(_, _, t) -> t) $ filter (\(f, _, _) -> f == from) transitions

instance Show NFA where
  show (NFA states labels start finishes delta) =
    "states = {"
      ++ join "," states
      ++ "}\n"
      ++ "input_alphabet = {"
      ++ join "," labels
      ++ "}\n"
      ++ "start_state = "
      ++ show start
      ++ "\n"
      ++ "accept_states = {"
      ++ join "," finishes
      ++ "}\n"
      ++ show delta

data DFA = DFA [StateSet] [Label] StateSet [StateSet] DFADelta

getNeighborsDfa :: [(StateSet, Label, StateSet)] -> StateSet -> [StateSet]
getNeighborsDfa transitions from = map (\(_, _, t) -> t) $ filter (\(f, _, _) -> f == from) transitions

instance Show DFA where
  show (DFA states labels start finishes delta) =
    "states = {"
      ++ join "," states
      ++ "}\n"
      ++ "input_alphabet = {"
      ++ join "," labels
      ++ "}\n"
      ++ "start_state = "
      ++ show start
      ++ "\n"
      ++ "accept_states = {"
      ++ join "," finishes
      ++ "}\n"
      ++ show delta
