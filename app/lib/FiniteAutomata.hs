module FiniteAutomata where

import Utils

newtype State = State String

instance Show State where
  show (State str) = str

instance Eq State where
  (==) (State x) (State y) = x == y

data Label = Eps | Label String

instance Show Label where
  show Eps = "_"
  show (Label str) = str

instance Eq Label where
  (==) (Label x) (Label y) = x == y
  (==) Eps Eps = True
  (==) _ _ = False

newtype NFADelta = NFADelta [(State, Label, [State])]

instance Show NFADelta where
  show (NFADelta transitions) =
    "delta = \n"
      ++ concatMap
        ( \(from, label, to) ->
            "\t" ++ show from ++ "," ++ show label ++ " -> " ++ join "," to ++ ";\n"
        )
        transitions

newtype DFADelta = DFADelta [(State, Label, State)]

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
