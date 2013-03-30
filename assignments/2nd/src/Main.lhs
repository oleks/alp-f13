\begin{code}
module Main where

import Grammar
import Parser
import Resolver

import System.Environment
import System.IO(hFlush, stdout)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> fail "Please provide a path to the initial program."
    [path] -> interpret path
    _ -> fail "Please provide at most one command-line argument."

interpret :: String -> IO ()
interpret path = do
  program <- parseProgramFile path
  putStrLn $ show program
  prompt program

prompt :: Program -> IO ()
prompt program = do
  putStr "?- "
  hFlush stdout
  queryString <- getLine

  let query = parseQuery ("?-" ++ queryString)
  let solution = solve program query

  showResult solution
  hFlush stdout
 
  prompt program

showResult :: [Env] -> IO ()
showResult [] = do
  putStrLn "no"
  hFlush stdout
showResult (e : es) = do
  putStr $ (show e) ++ " "
  hFlush stdout
  continue <- getLine
  case continue of
    ";" -> showResult es
    _ -> do
      putStrLn ""
      return ()

\end{code}
