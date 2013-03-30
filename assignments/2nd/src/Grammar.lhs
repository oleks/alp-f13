\section{Grammar}\label{sec:grammar}

\begin{code}
module Grammar(
    Value(..)
  --, Limb(..)
  , Clause(..)
  , Program(..)
  , Goal
  , Query(..)
  , Substitution
) where

import Data.Map(Map)
import Data.List(intercalate)

{-
data Limb = Infinite String | Nil
  deriving (Eq)
instance Show Limb where
  show (Infinite s) = "|" ++ s
  show _ = ""
-}

data Value
  = Variable String
  | Symbol String [Value]
--  | List [Value] Limb
  | Eq Value Value
  deriving (Eq)
instance Show Value where
  show (Variable v) = v
  show (Symbol "_nil" []) = "[]"
  show (Symbol "_cons" [x,xs]) =
    "[" ++ (show x) ++ (show1 xs) ++ "]"
    where
      show1 (Variable tn) = "|" ++ tn
      show1 (Symbol "_cons" [z, zs]) = "," ++ (show z) ++ (show1 zs)
      show1 (Symbol "_nil" []) = ""
      show1 v = show v
  show (Symbol x []) = x
  show (Symbol x vs) =
    x ++ "(" ++ (intercalate ", " $ map show vs) ++ ")"
{-  show (List es l) =
    "[" ++ (intercalate ", " $ map show es) ++ (show l) ++ "]"-}
  show (Eq v1 v2) =
    (show v1) ++ " = " ++ (show v2)

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
