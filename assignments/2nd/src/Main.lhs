\section{Main}\label{section:main}

\begin{code}
module Main where
\end{code}

\ignore{
\begin{code}
import Grammar
import Parser
import Resolver

import Data.List(intercalate)
import Data.Map(toList)
import System.Environment
import System.IO(hFlush, stdout)
\end{code}
}

The program requires for \emph{exactly} one argument to be given at start up.
This is because there currently no way to update the model dynamically.

\begin{code}
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> fail "Please provide a path to the initial program."
    [path] -> interpret path
    _ -> fail "Please provide at most one command-line argument."
\end{code}

If the user specifies a valid Pl file, the interpreter prints the module and
moves on on to the prompt, otherwise it dies.

\begin{code}
interpret :: String -> IO ()
interpret path = do
  program <- parseModuleFile path
  putStrLn $ show program
  prompt program
\end{code}

The interpreter is an iterative prompt that asks for goals to match. If the
user puts in an invalid query (syntax error, or unquantified variables), the
result is undefined.

\begin{code}
prompt :: Module -> IO ()
prompt program = do
  putStr "?- "
  hFlush stdout
  queryString <- getLine

  let query = parseQuery ("?-" ++ queryString)
  let solution = solve program query

  printSolution solution
  hFlush stdout
 
  prompt program
\end{code}

\texttt{printSolution} is a sort of secondary prompt. It will iterate over the
solutions and print them so long as the user asks for them (semicolon followed
by a line break). The function terminates as soon as there are no more results
to show, or the user types something other than a semicolon before performing a
line break.

\begin{code}
printSolution :: [Theta] -> IO ()
printSolution [] = do
  putStrLn "no"
  hFlush stdout
printSolution (e:es) = do
  putStr $ (showSubstitution e) ++ " "
  hFlush stdout
  continue <- getLine
  case continue of
    ";" -> printSolution es
    _ -> do
      putStrLn ""
      return ()
\end{code}

The code for showing a substituion is straight forward:

\begin{code}
showSubstitution :: Theta -> String
showSubstitution t =
  intercalate "\n" $ map (\(x, v) -> x ++ " = " ++ (show v)) (toList t)
\end{code}
