module FiniteAutomata where

import Utils

data State = State String | StateSet [String]

belongs :: State -> State -> Bool
belongs (StateSet a) (StateSet b) = all (`elem` b) a
belongs (State a) (StateSet b) = a `elem` b
belongs (State a) (State b) = a == b
belongs _ _ = False

merge :: State -> State -> State
merge (StateSet a) (StateSet b) = StateSet $ removeDuplicates (a ++ b)
merge (StateSet a) (State b) = merge (State b) (StateSet a)
merge (State a) (StateSet b) = if a `elem` b then StateSet b else StateSet (a : b)
merge (State a) (State b) = if a == b then State a else StateSet [a, b]

mergeAll :: [State] -> State
mergeAll [] = State "empty"
mergeAll (x : xs) = foldl merge x xs

instance Show State where
  show (State str) = str
  show (StateSet states) = joinStrings "" (sort states)

instance Eq State where
  (==) (StateSet a) (StateSet b) = all (`elem` b) a && all (`elem` a) b
  (==) (State a) (State b) = a == b
  (==) _ _ = False

data Label = Eps | Label String

instance Show Label where
  show Eps = "_"
  show (Label str) = str

instance Eq Label where
  (==) (Label x) (Label y) = x == y
  (==) Eps Eps = True
  (==) _ _ = False

newtype NFADelta = NFADelta [(State, Label, [State])]

resolveTransitionNfa :: NFADelta -> State -> Label -> [State]
resolveTransitionNfa (NFADelta transitions) from label = case result of
  [] -> []
  (x : _) -> x
  where
    result = map (\(_, _, t) -> t) $ filter (\(f, s, _) -> from == f && s == label) transitions

instance Show NFADelta where
  show (NFADelta transitions) =
    "delta = \n"
      ++ concatMap
        ( \(from, label, to) ->
            "\t" ++ show from ++ "," ++ show label ++ " -> " ++ join "," to ++ ";\n"
        )
        transitions

newtype DFADelta = DFADelta [(State, Label, State)]

resolveTransitionDfa :: DFADelta -> State -> Label -> State
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

data DFA = DFA [State] [Label] State [State] DFADelta

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
