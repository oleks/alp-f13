\section{Liveness Analysis}\label{sec:liveness}

Liveness analysis is implemented in the module \texttt{Liveness}. The result
type of a liveness analysis is a showable type. The user can therefore execute
an analysis from a Haskell prompt by loading the \texttt{Liveness} module, and
calling one of the exported functions. For instance,

\begin{itemize}

\item \texttt{> analyseFile "test1.il"} to parse, perform liveness analysis and
show the result for a program stored in \texttt{./test1.il}; or

\item \texttt{> analyseProgram p} to perform liveness analysis and show the
result for an already parsed program stored in \texttt{p}.

\end{itemize}

\begin{code}
module Liveness(
    analyseFile
  , analyseProgram
  , Liveness(..)
) where
\end{code}

\ignore{
\begin{code}
import Grammar
import Parser
\end{code}

\begin{code}
import Data.List(intercalate)
import Data.Map(Map,(!), insert)
import qualified Data.Map as Map
import Data.Array(Array, bounds, assocs, elems)
import Data.Set(Set, empty, singleton, fromList, unions, union, difference, toList)
\end{code}
}

\subsection{Helpful type aliases}

For each function in the program, the analysis produces an $out[i]$ and $in[i]$
set for each instruction $i$ in the function. We can represent the result as a
tuple of two maps from integers to a sets of identifiers. We define a type
alias for this single function result type. We also define a type alias for the
label map in the body of a function definition.

\begin{code}
type LabelMap = Map LabelId Int
type OutMap = Map Int (Set Id)
type InMap = Map Int (Set Id)
type OutInMaps = (OutMap, InMap)
\end{code}

\subsection{Results and pretty printing}

We define a program-wide liveness result type in order to provide pretty
printing functionality. The pretty printer prints the functions in a program as
a seqeunce of enumerated instructions followed by the final $out[i]$ and
$in[i]$ sets for each instruction $i$.

\begin{code}
data Liveness = Liveness [(Function, OutInMaps)]
instance Show Liveness where
  show (Liveness analysis) =
    intercalate "\n" $ map showFAnalysis analysis

showFAnalysis :: (Function, OutInMaps) -> String
showFAnalysis (Function h (Body a _), outInMaps) =
  let
    align =
      foldl (\x y -> (max . length . show) y x) 0 (elems a)
    instructions = 
      intercalate "\n" $ map (showIAnalysis align outInMaps) (assocs a)
  in (show h) ++ "\n[\n" ++ instructions ++ "\n]\n"

showIAnalysis :: Int -> OutInMaps -> (Int, Instruction) -> String
showIAnalysis align (outMap, inMap) (i, ins) =
  (show i) ++ "\t" ++ (pad (show ins) align) ++ "\t" ++
    (intercalate " " $ map show $ toList $ outMap ! i) ++ "\t\t" ++
    (intercalate " " $ map show $ toList $ inMap ! i)

pad :: String -> Int -> String
pad s width =
  let diff = width - length s
  in if diff > 0 then s ++ (take diff $ repeat ' ') else s
\end{code}

\subsection{Initializing the analysis}

The analysis is called anew for each function in the program; the analysis does
not cross function boundaries. For each function, the analysis starts out with
empty $in[i]$ and $out[i]$ sets for each instruction $i$. These sets are then
filled using a fixed-point iteration in an order reverse of the instruction
order, as suggested in the litterature \cite[Page 41]{torben}.

\begin{code}
analyseFile :: FilePath -> IO Liveness
analyseFile path = do
  program <- parseFile path
  return $ analyseProgram program

analyseProgram :: Program -> Liveness
analyseProgram (Program lfs) = do
  let fs = list1list lfs
  Liveness $ zip fs $ map analyseFunction fs

analyseFunction :: Function -> OutInMaps
analyseFunction (Function _ (Body a m)) = do
  let revIns = reverse $ assocs a
  fixedPointIter revIns m (emptyOutInMaps a)

emptyOutInMaps :: Array Int Instruction -> OutInMaps
emptyOutInMaps a =
  let
    (minI, maxI) = bounds a
    emptyIdSet = Map.fromList [ (i, empty) | i <- [minI..maxI] ]
  in
    (emptyIdSet, emptyIdSet)
\end{code}

\subsection{Fixed-point iteration}

Fixed-point iteration is implemented by iteratively computing a new $in[i]$ and
$out[i]$ set for each instruction $i$ in the given list of instructions. If at
some point in an otherwise infinite recursion the new collection of $in[i]$ and
$out[i]$ sets is equal to the old collection, the fixed-point iteration returns
the old collection.

\begin{code}
fixedPointIter
  :: [(Int, Instruction)] -> LabelMap -> OutInMaps -> OutInMaps
fixedPointIter a m s =
  let s1 = foldl (iter m) s a
  in  if s == s1
      then s
      else fixedPointIter a m s1

iter :: LabelMap -> OutInMaps -> (Int, Instruction) -> OutInMaps
iter m (outMap, inMap) x @ (i, _) =
  let
    outMap' = insert i (nextOutSet m inMap x) outMap
    inMap' = insert i (nextInSet outMap x) inMap
  in
    (outMap', inMap')
\end{code}

\subsection{Generating new $in[i]$ and $out[i]$ sets}

We generate new $in[i]$ and $out[i]$ sets for a particular instruction $i$ as
defined by the recursive formulas \cite[3.1 and 3.2 (p. 40)]{torben}. We repeat
the formulas here for readability. To avoid infinite recursion, the actual
function definition take in an original $in[i]$ set our $out[i]$ set, and
produce their outcome based on the values in those.

\begin{align*}
in[i] &= gen[i] \cup \p{ out[i] \setminus kil[i] } \\
out[i] &= \bigcup_{j \in succ[i]} in[j]
\end{align*}

\begin{code}
nextOutSet :: LabelMap -> InMap -> (Int, Instruction) -> Set Id
nextOutSet m inMap x =
  unions $ map (inMap !) $ succ' m x

nextInSet :: OutMap -> (Int, Instruction) -> Set Id
nextInSet outMap (i, ins) =
  union (gen ins) (difference (outMap ! i) (kill ins))
\end{code}

\subsection{The $gen[i]$ and $kill[i]$ sets}

We define the $gen[i]$ and $kill[i]$ sets for a particular instruction $i$, as
defined in \cite[Figure 3.2 (p. 39)]{torben}.

\begin{code}
gen :: Instruction -> Set Id
gen (Assign _ (AtomId y))           = fromList [y]
gen (AssignUnop _ _ (AtomId y))     = fromList [y]
gen (AssignBinop _ y _ (AtomId z))  = fromList [y, z]
gen (AssignBinop _ y _ _)           = fromList [y]
gen (Load _ (AtomId y))             = fromList [y]
gen (Store (AtomId x) y)            = fromList [x, y]
gen (Store _ y)                     = fromList [y]
gen (IfThenElse x _ (AtomId y) _ _) = fromList [x, y]
gen (AssignCall _ _ (Args args))    = fromList (list1list args)
gen (Return x)                      = fromList [x]
gen _                               = empty

kill :: Instruction -> Set Id
kill (Assign x _)                   = singleton x
kill (AssignUnop x _ _)             = singleton x
kill (AssignBinop x _ _ _)          = singleton x
kill (Load x _)                     = singleton x
kill (AssignCall x _ _)             = singleton x
kill _                              = empty
\end{code}

\subsection{The $succ[i]$ set}

We define the $succ[i]$ set for a particular instruction $i$, as defined at the
top of \cite[Page 39)]{torben}.

This function uses the partial function \texttt{(!)} to lookup the
corresponding instruction index in the label map, i.e. it assumes that there
are no jumps to nonexisting labels. This function also assumes that if the
instruction is neither a \texttt{GOTO}, an \texttt{IF-THEN-ELSE} or a
\texttt{RETURN}, then it is not the last instruction in a function definition.
Both of the above assumptions hold by definition of IL. The return type is a
list as the set will merely be traversed.

\begin{code}
succ' :: LabelMap -> (Int, Instruction) -> [Int]
succ' m (i, ins) =
  case ins of
    Goto l                  -> [m ! l]
    IfThenElse _ _ _ l1 l2  -> [m ! l1, m ! l2]
    Return _                -> []
    _                       -> [i + 1]
\end{code}
