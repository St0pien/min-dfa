module Main where

import FiniteAutomata (NFA)
import Parsing (parseInput)
import System.Environment (getArgs)
import System.IO (Handle, IOMode (ReadMode), hClose, hGetLine, hIsEOF, openFile)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then
      displayHelp
    else
      run (head args)

displayHelp :: IO ()
displayHelp = do
  putStrLn $ red "[-]" ++ " Usage: min-dfa [path to input file]"

run :: String -> IO ()
run inputPath = do
  inputFile <- openFile inputPath ReadMode
  inputLines <- readLines inputFile
  startMinimizing (parseInput inputLines)
  hClose inputFile

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

startMinimizing :: Either String NFA -> IO ()
startMinimizing (Left err) = do
  putStrLn $ red "[-]" ++ " Parsing error: \n\t" ++ err
startMinimizing (Right nfa) = do
  putStrLn $ green "[+]" ++ " Parsing successfull"
  putStrLn $ green "[+]" ++ " Loaded automata:\n---"
  print nfa
  putStrLn "---"
  putStrLn $ green "[+]" ++ " Minimizing starts"

red :: String -> String
red str = "\ESC[31m" ++ str ++ "\ESC[0m"

green :: String -> String
green str = "\ESC[32m" ++ str ++ "\ESC[0m"
