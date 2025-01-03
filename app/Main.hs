module Main where

import Determinisation
import EpsRemoval
import FiniteAutomata
import Minimization
import Parsing (parseInput)
import System.Environment (getArgs)
import System.IO (Handle, IOMode (ReadMode, WriteMode), hClose, hGetLine, hIsEOF, hPrint, openFile)
import UnachieveableRemoval
import Utils (join)

main :: IO ()
main = do
  args <- getArgs
  if length args < 2
    then
      displayHelp
    else
      run args

displayHelp :: IO ()
displayHelp = do
  putStrLn $ red "[-]" ++ " Usage: min-dfa [path to input file] [path to output file]"

run :: [String] -> IO ()
run [inputPath, outpuPath] = do
  inputFile <- openFile inputPath ReadMode
  inputLines <- readLines inputFile
  startMinimizing (parseInput inputLines) outpuPath
  hClose inputFile
run _ = return ()

readLines :: Handle -> IO [String]
readLines file = do
  eof <- hIsEOF file
  if eof
    then
      return []
    else do
      line <- hGetLine file
      rest <- readLines file
      return (line : rest)

startMinimizing :: Either String NFA -> String -> IO ()
startMinimizing (Left err) _ = do
  putStrLn $ red "[-]" ++ " Parsing error: \n\t" ++ err
startMinimizing (Right epsNfa) outputPath = do
  putStrLn $ green "[+]" ++ " Parsing successfull"
  putStrLn $ green "[+]" ++ " Loaded automata:\n---"
  print epsNfa
  putStrLn "---"
  putStrLn $ green "[+]" ++ " Minimizing starts"

  let achievableNfa = removeUnachieveableNfa epsNfa
  printUnusedSymbols epsNfa achievableNfa
  printUnachieveableStates epsNfa achievableNfa
  putStrLn $ green "[+]" ++ " Automata without unnecessary elements:\n---"
  print achievableNfa
  putStrLn "---"
  let nfa = removeEpsTransitions achievableNfa
  putStrLn $ green "[+]" ++ " Automata without epsilon transitions:\n---"
  print nfa
  putStrLn "---"
  let dfa = determinise nfa
  putStrLn $ green "[+]" ++ " Corresponding DFA before minimization:\n---"
  print dfa
  putStrLn "---"

  let mindfa = minimize dfa
  putStrLn $ green "[+]" ++ " Minimization successful writing output to: " ++ outputPath
  saveOutput mindfa outputPath

red :: String -> String
red str = "\ESC[31m" ++ str ++ "\ESC[0m"

green :: String -> String
green str = "\ESC[32m" ++ str ++ "\ESC[0m"

printUnusedSymbols :: NFA -> NFA -> IO ()
printUnusedSymbols before after = do
  let (NFA _ inputSymbols _ _ _) = before
  let (NFA _ achSymbols _ _ _) = after
  putStrLn $ green "[+]" ++ " Unused symbols: " ++ join "," (filter (`notElem` achSymbols) inputSymbols)

printUnachieveableStates :: NFA -> NFA -> IO ()
printUnachieveableStates before after = do
  let (NFA inputStates _ _ _ _) = before
  let (NFA achStates _ _ _ _) = after
  putStrLn $ green "[+]" ++ " Unachieveable states: " ++ join "," (filter (`notElem` achStates) inputStates)

saveOutput :: DFA -> String -> IO ()
saveOutput dfa outputPath = do
  handle <- openFile outputPath WriteMode
  hPrint handle dfa
  hClose handle
