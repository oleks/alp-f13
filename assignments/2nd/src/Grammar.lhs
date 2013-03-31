\section{Grammar}\label{section:grammar}

This module defines the Pl grammar.

\begin{code}
module Grammar(
    Value(..)
  , Clause(..)
  , Module(..)
  , Goal
  , Query(..)
  , Theta
) where
\end{code}

\ignore{
\begin{code}
import Data.Map(Map)
import Data.List(intercalate)
\end{code}
}

A Pure Prolog value, or proposition, is either a variable or a symbol with 0 or
more values as arguments. Pl also has several extensions, among them ability
compare a pair of values for equality, and cuts. The value data type in Pl is
defined as follows:

\begin{code}
data Value
  = Variable String
  | Symbol String [Value]
  | Eq Value Value
  | Cut
  deriving (Eq)
\end{code}

Pl values can also be lists. These are implemented by translating list notation
into Pure Prolog notation:

\begin{itemize}

\item \texttt{[]} is rewritten to \texttt{\_nil},

\item \texttt{[A,..,Z|Zs]} is rewritten to
\texttt{\_cons(A,..,\_cons(Z,Zs)..)}, and

\item \texttt{[A,..,Y,Z]} is rewritten to
\texttt{\_cons(A,..,\_cons(Y,\_cons(Z,\_nil))..)}.

\end{itemize}

As \texttt{\_} is not a valid starting character for either variables or
symbols, this transformation does not limit the user in any way.

Negation as failure could be implemented in a similar way: We wrap every
negated proposition in a unary predicate \texttt{\_not}, and define auxiliary
clauses as specified in \cite[\textsection 7.5.2 (p. 138)]{torben}.

As will be shown in \referToSection{parser}, equality is implemented as an
infix right-associative operator with the lowest precedence.  To mitigate for
user confusion regarding this fact, comparison for equality is always shown in
explicit parentheses, so \texttt{A=B=C} is always interpreted and shown as
\texttt{(A=(B=C))}.

The two points above explain the extended show function for the value data
type:

\begin{code}
instance Show Value where
  show (Variable v) = v
  show (Symbol "_nil" []) = "[]"
  show (Symbol "_cons" [x,xs]) =
    "[" ++ (show x) ++ (show' xs) ++ "]"
    where
      show' (Variable tn)
        = "|" ++ tn
      show' (Symbol "_cons" [z, zs])
        = "," ++ (show z) ++ (show' zs)
      show' (Symbol "_nil" []) = ""
      show' v = show v
  show (Symbol x []) = x
  show (Symbol x vs) =
    x ++ "(" ++ (intercalate ", " $ map show vs) ++ ")"
  show (Eq v1 v2) =
    "(" ++ (show v1) ++ " = " ++ (show v2) ++ ")"
  show Cut = "!"
\end{code}

A Pure Prolog clause has a value, or proposition, on the left-hand side and 0
or more values, or propositions on the right hand side of a turnstile:

\begin{code}
data Clause = Clause Value [Value]
  deriving (Eq)
instance Show Clause where
  show (Clause l []) = (show l) ++ "."
  show (Clause l rs) =
    (show l) ++ " :- " ++ (intercalate ", " $ map show rs) ++ "."
\end{code}

A Pure Prolog program is a list of clauses:

\begin{code}
data Module = Module [Clause]
  deriving (Eq)
instance Show Module where
  show (Module cs) = intercalate "\n" $ map show cs
\end{code}

A Pure Prolog query is a list of goals, which themselves are merely values:

\begin{code}
type Goal = Value

data Query = Query [Goal]
  deriving (Eq)
instance Show Query where
  show (Query ps) = "?- " ++ (intercalate ", " $ map show ps) ++ "."
\end{code}

Substitutions in \cite[\textsection 7.2 (p. 129)]{torben} are referred to by
the Greek letter $\Theta$, hence the choice of naming for substitutions:

\begin{code}
type Theta = Map String Value
\end{code}
