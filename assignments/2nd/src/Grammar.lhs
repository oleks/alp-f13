\section{Grammar}\label{sec:grammar}

\begin{code}
module Grammar(
    Value(..)
  , Clause(..)
  , Program(..)
  , Query(..)
) where

import Data.List(intercalate)

data Value = Value String [Value]
instance Show Value where
  show (Value x []) = x
  show (Value x vs) =
    x ++ "(" ++ (intercalate ", " $ map show vs) ++ ")"

data Clause = Clause Value [Value]
instance Show Clause where
  show (Clause l []) = (show l) ++ "."
  show (Clause l rs) =
    (show l) ++ " :- " ++ (intercalate ", " $ map show rs) ++ "."

data Program = Program [Clause]
instance Show Program where
  show (Program cs) = intercalate "\n" $ map show cs

data Query = Query [Value]
instance Show Query where
  show (Query ps) = "?- " ++ (intercalate ", " $ map show ps) ++ "."
\end{code}
