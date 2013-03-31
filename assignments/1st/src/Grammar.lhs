\section{Grammar}\label{sec:grammar}

The module \texttt{Grammar} defines the value types of the IL grammar and its
various components.

\begin{code}
module Grammar (
    List1(..),      list1list
  , Program(..),    Function(..),     Header(..),   Args(..)
  , FunctionId(..), Id(..),           LabelId(..)
  , Body(..),       Instruction(..)
  , Atom(..),       Unop(..),         Binop(..),    Relop(..)
) where
\end{code}

\ignore{
\begin{code}
import Data.List(intercalate)
import Data.Map(Map)
import Data.Array(Array, elems)
\end{code}
}

The module defines a special list type, \texttt{List1}, requiring at least one
element in the list. This structure is employed in order to constructively
maintain invariants such as, a program in IL consists of at least one function,
or a list of arguments consists of at least one argument.

\begin{code}
data List1 a = Head a (List1 a) | Limb a

list1list :: List1 a -> [a]
list1list (Head a as) = a : (list1list as)
list1list (Limb a) = [a]

list1map :: (a -> b) -> List1 a -> [b]
list1map f (Head a as) = (f a) : (list1map f as)
list1map f (Limb a) = [f a]
\end{code}

We let each value type of a grammar component derive from the type class
\texttt{Show}, allowing us to to easily show programs in human-readable format.

As already mentioned above, a program in IL is a list of functions of length at
least 1:

\begin{code}
data Program
  = Program (List1 Function)
instance Show Program where
  show (Program fs) = unlines (list1map show fs)
\end{code}

A function consists of a header and a body. When showing a function, we
separate its constituents by a line break for readability.

\begin{code}
data Function
  = Function Header Body
instance Show Function where
  show (Function h b) = (show h) ++ "\n" ++ (show b)
\end{code}

A header consists of a function identifier and some arguments enclosed in
parentheses.

\begin{code}
data Header = Header FunctionId Args
instance Show Header where
  show (Header fid args) = (show fid) ++ "(" ++ (show args) ++ ")"
\end{code}

The body of a function is a list of instructions of length at least one. This
invariant is lifted from the type level as the \texttt{List1} type hinders
efficient execution and analysis of IL. Additionally, to aid execution and
analysis, the body has a map from labels to indexes into the array of
instructions. This allows e.g. $O(1)$ time jumps. When shown, instructions are
enclosed in square brackets and line breaks, and separated by commas and
line breaks.

\begin{code}
data Body = Body (Array Int Instruction) (Map LabelId Int)
instance Show Body where
  show (Body is _) = "[\n  " ++
    (intercalate ",\n  " $ map show $ elems is) ++
    "\n]\n"
\end{code}

\begin{code}
data Instruction
  = Label LabelId
  | Goto LabelId
  | Return Id
  | Load Id Atom
  | Store Atom Id
  | Assign Id Atom
  | AssignUnop Id Unop Atom
  | AssignBinop Id Id Binop Atom
  | AssignCall Id FunctionId Args
  | IfThenElse Id Relop Atom LabelId LabelId
instance Show Instruction where
  show (Label i)    = "LABEL " ++ (show i)
  show (Goto l)     = "GOTO " ++ (show l)
  show (Return i)   = "RETURN " ++ (show i)
  show (Load i a)   = (show i) ++ " := " ++ "M[" ++ (show a) ++ "]"
  show (Store a i)  = "M[" ++ (show a) ++ "]" ++ " := " ++ (show i)
  show (Assign i a) = (show i) ++ " := " ++ (show a)
  show (AssignUnop i u a) =
    let (is, us, as) = (show i, show u, show a)
    in is ++ " := " ++ us ++ as
  show (AssignBinop r i b a) =
    let (rs, is, bs, as) = (show r, show i, show b, show a)
    in rs ++ " := " ++ is ++ " " ++ bs ++ " " ++ as
  show (AssignCall i fid args) =
    let (is, fids, argss) = (show i, show fid, show args)
    in is ++ " := CALL " ++ fids ++ "(" ++ argss ++ ")"
  show (IfThenElse i r a l1 l2) =
    let (is, rs, as, l1s, l2s) =
          (show i, show r, show a, show l1, show l2)
    in "IF " ++ is ++ " " ++ rs ++ " " ++ as ++
        " THEN " ++ l1s ++ " ELSE " ++ l2s
\end{code}

An argument list is a list of identifiers of at least one element, separated by
commas.

\begin{code}
data Args = Args (List1 Id)
instance Show Args where
  show (Args ids) = intercalate ", "  $ list1map show ids
\end{code}

Although identifiers in general may be regarded as equal, we choose to continue
the distinction drawn in the litterature between variable, function and label
identifiers. We let identifiers automatically derive from the type classes
\texttt{Eq} and \texttt{Ord}, so that we can compare and order them.

\begin{code}
data Id = Id String
  deriving(Eq, Ord)
instance Show Id where
  show (Id s) = s

data FunctionId = FunctionId String
  deriving(Eq, Ord)
instance Show FunctionId where
  show (FunctionId s) = s

data LabelId = LabelId String
  deriving(Eq, Ord)
instance Show LabelId where
  show (LabelId s) = s
\end{code}

An atomic expression is either a variable or a constant.

\begin{code}
data Atom = AtomId Id | AtomNum Int

instance Show Atom where
  show (AtomId  i)  = show i
  show (AtomNum n)  = show n
\end{code}

Below we define a couple standard unary, binary and relational operators.

\begin{code}
data Unop = Neg
instance Show Unop where
  show Neg = "~"

data Binop = Add | Sub | Mul | And | Or | Xor
instance Show Binop where
  show Add  = "+"
  show Sub  = "-"
  show Mul  = "*"
  show And  = "&"
  show Or   = "|"
  show Xor  = "^"

data Relop = Eq | Neq | Lt | Gt | Leq | Geq
instance Show Relop where
  show Eq   = "="
  show Neq  = "!="
  show Leq  = "<="
  show Geq  = ">="
  show Lt   = "<"
  show Gt   = ">"
\end{code}
