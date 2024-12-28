module FiniteAutomata where

import Utils

newtype State = State String

instance Show State where
  show (State str) = str

data Label = Eps | Label String

instance Show Label where
  show Eps = "_"
  show (Label str) = str

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

q = State "q"

q2 = State "q2"

a = Label "a"

n = NFADelta [(q, a, [q2, q]), (q, Eps, [q2])]

d = DFADelta [(q, a, q)]

na = NFA [q, q2] [a, Eps] q [q2] n

da = DFA [q, q2] [a] q [q2] d
