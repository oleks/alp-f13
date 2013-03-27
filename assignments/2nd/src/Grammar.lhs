\section{Grammar}\label{sec:grammar}

\begin{code}
module Grammar(
    Value(..)
  , Clause(..)
  , Program(..)
  , Goal
  , Query(..)
  , Substitution
) where

import Data.Map(Map)
import Data.List(intercalate)

data Value = Variable String | Symbol String [Value]
  deriving (Eq)
instance Show Value where
  show (Variable v) = v
  show (Symbol x []) = x
  show (Symbol x vs) =
    x ++ "(" ++ (intercalate ", " $ map show vs) ++ ")"

data Clause = Clause Value [Value]
  deriving (Eq)
instance Show Clause where
  show (Clause l []) = (show l) ++ "."
  show (Clause l rs) =
    (show l) ++ " :- " ++ (intercalate ", " $ map show rs) ++ "."

data Program = Program [Clause]
  deriving (Eq)
instance Show Program where
  show (Program cs) = intercalate "\n" $ map show cs

type Goal = Value

data Query = Query [Goal]
  deriving (Eq)
instance Show Query where
  show (Query ps) = "?- " ++ (intercalate ", " $ map show ps) ++ "."

type Substitution = Map String Value
\end{code}
