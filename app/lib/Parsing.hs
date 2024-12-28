module Parsing where

import FiniteAutomata
import Utils (split)

parseInput :: [String] -> Either String NFA
parseInput lns
  | not (null nonExistingStates) = Left ("Unkown states used in delta function: " ++ show nonExistingStates)
  | not (null nonExistingSymbols) = Left ("Unkown symbols used in delta function: " ++ show nonExistingSymbols)
  | otherwise = Right $ NFA (map State states) (mapLabels alphabet) (State start) (map State accepts) (mapDelta delta)
  where
    code = removeNoops lns
    states = parseStates code
    alphabet = parseInputAlphabet code
    start = parseStartState code
    accepts = parseAcceptStates code
    delta = parseDelta code
    nonExistingStates = findNonExistingStates delta states
    nonExistingSymbols = findNonExistingSymbols delta alphabet

variableCharset :: String
variableCharset = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '1'] ++ "_"

tokenCharset :: String
tokenCharset = variableCharset ++ "={},;->"

removeNoops :: [String] -> String
removeNoops = concat . filter (not . null) . map (filter (`elem` tokenCharset) . takeWhile (/= '/'))

parseStates :: String -> [String]
parseStates = split "," . takeWhile (/= '}') . (!! 1) . split "states={"

parseInputAlphabet :: String -> [String]
parseInputAlphabet = split "," . takeWhile (/= '}') . (!! 1) . split "input_alphabet={"

parseStartState :: String -> String
parseStartState = head . split "accept_states" . (!! 1) . split "start_state="

parseAcceptStates :: String -> [String]
parseAcceptStates = split "," . takeWhile (/= '}') . (!! 1) . split "accept_states={"

parseDelta :: String -> [(String, String, [String])]
parseDelta = map parseTransition . init . split ";" . (!! 1) . split "delta="

parseTransition :: String -> (String, String, [String])
parseTransition transition =
  let arrowSplit = split "->" transition
      commaSplit = split "," (head arrowSplit)
   in (head commaSplit, commaSplit !! 1, split "," (arrowSplit !! 1))

findNonExistingStates :: [(String, String, [String])] -> [String] -> [String]
findNonExistingStates delta states =
  let from = map (\(f, _, _) -> f) delta
      to = concatMap (\(_, _, t) -> t) delta
   in filter (`notElem` states) (from ++ to)

findNonExistingSymbols :: [(String, String, [String])] -> [String] -> [String]
findNonExistingSymbols delta symbols = filter (\s -> (s /= "_") && (s `notElem` symbols)) $ map (\(_, s, _) -> s) delta

mapLabels :: [String] -> [Label]
mapLabels = map (\l -> if l == "_" then Eps else Label l)

mapDelta :: [(String, String, [String])] -> NFADelta
mapDelta delta =
  NFADelta $
    map
      ( \(from, symbol, to) ->
          (State from, if symbol == "_" then Eps else Label symbol, map State to)
      )
      delta
