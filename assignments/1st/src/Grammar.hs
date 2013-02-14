module Grammar (
    List1(..)
  , list1list
  , Program(..)
  , Function(..)
  , Header(..)
  , FunctionId(..)
  , Id(..)
  , Body(..)
  , Instruction(..)
  , LabelId(..)
  , Atom(..)
  , Unop(..)
  , Binop(..)
  , Relop(..)
) where

import Data.List(intersperse)

data List1 a
  = Head a (List1 a) | Limb a

list1list :: List1 a -> [a]
list1list (Head a as) = a : (list1list as)
list1list (Limb a) = [a]

list1map :: (a -> b) -> List1 a -> [b]
list1map f (Head a as) = (f a) : (list1map f as)
list1map f (Limb a) = [f a]

data Program
  = Program (List1 Function)

instance Show Program where
  show (Program fs) = unlines (list1map show fs)

data Function
  = Function Header Body

instance Show Function where
  show (Function h b) = (show h) ++ "\n" ++ (show b)


data Header
  = Header FunctionId (List1 Id)

showSignature :: FunctionId -> (List1 Id) -> String
showSignature fid ids =
  let
    args = concat $ intersperse "," (list1map show ids)
  in
    (show fid) ++ "(" ++ args ++ ")"

instance Show Header where
  show (Header fid ids) = showSignature fid ids


data FunctionId
  = FunctionId String

instance Show FunctionId where
  show (FunctionId s) = s


data Id
  = Id String

instance Show Id where
  show (Id s) = s


data Body = Body (List1 Instruction)

instance Show Body where
  show (Body is) = "[\n  " ++
    (concat (intersperse "\n  " (list1map show is))) ++
    "\n]\n"

data Instruction
  = Label LabelId
  | Assign Id Atom
  | AssignUnop Id Unop Atom
  | AssignBinop Id Id Binop Atom
  | Load Id Atom
  | Store Atom Id
  | Goto LabelId
  | IfThenElse Id Relop Atom LabelId LabelId
  | AssignCall Id FunctionId (List1 Id)
  | Return Id

instance Show Instruction where
  show (Label i) = "LABEL " ++ (show i)
  show (Assign i a) = (show i) ++ " := " ++ (show a)
  show (AssignUnop i u a) = (show i) ++ " := " ++ (show u) ++ (show a)
  show (AssignBinop r i b a) =
    (show r) ++ " := " ++ (show i) ++ " " ++ (show b) ++ " " ++ (show a)
  show (Load i a) = (show i) ++ " := " ++ "M[" ++ (show a) ++ "]"
  show (Store a i) = "M[" ++ (show a) ++ "]" ++ " := " ++ (show i)
  show (Goto l) = "GOTO " ++ (show l)
  show (IfThenElse i r a l1 l2) =
    "IF " ++ (show i) ++ " " ++ (show r) ++ " " ++ (show a) ++
      " THEN " ++ (show l1) ++ " ELSE " ++ (show l2)
  show (AssignCall i fid ids) =
    (show i) ++ " :=  " ++ "CALL " ++ (showSignature fid ids)
  show (Return i) = "RETURN " ++ (show i)

data LabelId = LabelId String

instance Show LabelId where
  show (LabelId s) = s


data Atom
  = AtomId Id
  | AtomNum Int

instance Show Atom where
  show (AtomId  i)  = show i
  show (AtomNum n)  = show n


data Unop
  = Neg

instance Show Unop where
  show Neg = "~"


data Binop
  = Add
  | Sub
  | Mul
  | And
  | Or
  | Xor

instance Show Binop where
  show Add  = "+"
  show Sub  = "-"
  show Mul  = "*"
  show And  = "&"
  show Or   = "|"
  show Xor  = "^"


data Relop
  = Eq
  | Neq
  | Lt
  | Gt
  | Leq
  | Geq

instance Show Relop where
  show Eq   = "="
  show Neq  = "≠"
  show Lt   = "<"
  show Gt   = ">"
  show Leq  = "≤"
  show Geq  = "≥"
