\section{Parser}

The parser is implemented by transforming the grammar defined in \cite[Section
3.2 (p.  36)]{torben} into a parsing expression grammar, and implementing the
parser using the \texttt{ReadP} parser combinator. The code is relatively
straight-forward once in is informed that the \texttt{(<++)} operator is a
choice operator that only considers the parser to the right if the parser to
the left fails.

\begin{code}
module Parser(
  parse,
  parseFile
) where
\end{code}

\ignore{
\begin{code}
import Grammar
\end{code}

\begin{code}
import Data.Array(listArray, assocs)
import Data.Map(Map, empty, insert, member)
import Data.List(foldl')

import Text.ParserCombinators.ReadP
\end{code}
}

\subsection{Exported functions}

The user can either parse a program in a string or a program stored at a given
path.

\begin{code}
parse :: String -> Program
parse text =
  case readP_to_S programP text of
    [(p, "")] -> p
    x -> error $ show x

parseFile :: FilePath -> IO Program
parseFile filepath = do
  text <- readFile filepath
  return $ parse text
\end{code}

\subsection{Auxiliary parser combinators}

A couple generically useful parser combinators, extending the functionality of
\texttt{ReadP}.

\begin{code}
skip :: ReadP a -> ReadP ()
skip p = do
  _ <- p
  return ()

charToken :: Char -> ReadP ()
charToken c = do
  skipSpaces
  skip (char c)
  skipSpaces

token :: ReadP a -> ReadP a
token p = do
  skipSpaces
  v <- p
  skipSpaces
  return v

skipToken :: String -> ReadP ()
skipToken s = skip $ token $ string s

tokenV :: String -> a -> ReadP a
tokenV s v = do
  skipToken s
  return v

charof :: [Char] -> ReadP Char
charof cs = satisfy $ \c -> elem c cs

stringof :: [Char] -> [Char] -> ReadP String
stringof cs1 cs2 = token $ do
    c <- charof cs1
    cs <- many $ charof cs2
    return $ c : cs
\end{code}

\subsection{\texttt{List1} parser combinators}

The following correspond to the \texttt{many1} and \texttt{sebBy1} parser
combinators for lists, respectively.

\begin{code}
list1P :: ReadP a -> ReadP (List1 a)
list1P p = do
  v <- p
  do { vs <- list1P p; return $ Head v vs } <++ (return $ Limb v)

list1SepByP :: ReadP a -> ReadP sep -> ReadP (List1 a)
list1SepByP p sep = do
  v <- p
  do { _ <- sep; vs <- list1SepByP p sep; return $ Head v vs } <++ (return $ Limb v)
\end{code}

\subsection{IL parser combinators}

\begin{code}
programP :: ReadP Program
programP = do
  fs <- list1P functionP
  return $ Program fs

functionP :: ReadP Function
functionP = do
  h <- headerP
  b <- bodyP
  return $ Function h b
\end{code}

\begin{code}
headerP :: ReadP Header
headerP = do
  (fid, ids) <- signatureP
  return $ Header fid ids
\end{code}

\begin{code}
signatureP :: ReadP (FunctionId, Args)
signatureP = do
  fid <- functionIdP
  charToken '('
  ids <- list1SepByP idP (charToken ',')
  let args = Args ids
  charToken ')'
  return $ (fid, args)
\end{code}

\begin{code}
bodyP :: ReadP Body
bodyP = do
  charToken '['
  ins <- sepBy1 instructionP (optional $ charToken ',')
  let insArray = listArray (1, length ins) ins
  let labels = foldl' insertLabel empty (assocs insArray)
  charToken ']'
  return $ Body insArray $! labels
\end{code}

\begin{code}
insertLabel :: (Map LabelId Int) -> (Int, Instruction) -> (Map LabelId Int)
insertLabel s (i, ins) =
  case ins of
    Label l ->
      if member l s
      then
        error $ "Label " ++ (show l) ++ " occurs more than once!"
      else insert l i s
    _ -> s
\end{code}

\begin{code}
labelP :: ReadP Instruction
labelP = do
  skipToken "LABEL"
  labelId <- labelIdP
  return $ Label labelId

assignP :: ReadP Instruction
assignP = do
  i <- idP
  skipToken ":="
  assignUnopP i <++
    assignLoadP i   <++
    assignCallP i   <++
    assignBinopP i  <++
    assignAtomP i

assignAtomP :: Id -> ReadP Instruction
assignAtomP i = do
  a <- atomP
  return $ Assign i a

assignUnopP :: Id -> ReadP Instruction
assignUnopP i = do
  u <- unopP
  a <- atomP
  return $ AssignUnop i u a

assignLoadP :: Id -> ReadP Instruction
assignLoadP i = do
  a <- memoryP
  return $ Load i a

assignCallP :: Id -> ReadP Instruction
assignCallP i = do
  skipToken "CALL"
  (fid, args) <- signatureP
  return $ AssignCall i fid args

assignBinopP :: Id -> ReadP Instruction
assignBinopP r = do
  i <- idP
  b <- binopP
  a <- atomP
  return $ AssignBinop r i b a

storeP :: ReadP Instruction
storeP = do
  a <- memoryP
  skipToken ":="
  i <- idP
  return $ Store a i

memoryP :: ReadP Atom
memoryP = do
  skipSpaces
  skip $ string "M["
  a <- atomP
  skip $ string "]"
  skipSpaces
  return a

gotoP :: ReadP Instruction
gotoP = do
  skipToken "GOTO"
  l <- labelIdP
  return $ Goto l

returnP :: ReadP Instruction
returnP = do
  skipToken "RETURN"
  i <- idP
  return $ Return i

ifThenElseP :: ReadP Instruction
ifThenElseP = do
  skipToken "IF"
  i <- idP
  r <- relopP
  a <- atomP
  skipToken "THEN"
  l1 <- labelIdP
  skipToken "ELSE"
  l2 <- labelIdP
  return $ IfThenElse i r a l1 l2
\end{code}

\begin{code}
instructionP :: ReadP Instruction
instructionP = labelP <++ assignP <++ storeP <++ gotoP <++ returnP <++ ifThenElseP
\end{code}

\subsection{Identifiers}

\begin{code}
functionIdP :: ReadP FunctionId
functionIdP = do
  cs <- genericIdP
  return $ FunctionId cs

labelIdP :: ReadP LabelId
labelIdP = do
  cs <- genericIdP
  return $ LabelId cs

idP :: ReadP Id
idP = do
  cs <- genericIdP
  return $ Id cs

genericIdP :: ReadP String
genericIdP =
  stringof (['a'..'z'] ++ "_")  (['a'..'z'] ++ ['0'..'9'] ++ "_")
\end{code}

\subsection{Atoms}

\begin{code}
atomP :: ReadP Atom
atomP =
  do { i <- idP; return $ AtomId i } <++
    do { num <- numP; return $ AtomNum num }

numP :: ReadP Int
numP = do
  cs <- stringof ['0'..'9'] ['0'..'9']
  return $ read cs
\end{code}

\subsection{Unary, binary and relational operators}

\begin{code}
unopP :: ReadP Unop
unopP
  = tokenV "~" Neg

binopP :: ReadP Binop
binopP
  = tokenV "+" Add  <++
    tokenV "-" Sub  <++
    tokenV "*" Mul  <++
    tokenV "&" And  <++
    tokenV "|" Or   <++
    tokenV "^" Xor

relopP :: ReadP Relop
relopP
  = tokenV "="  Eq  <++
    tokenV "!=" Neq <++
    tokenV "<=" Leq <++
    tokenV ">=" Geq <++
    tokenV "<"  Lt  <++
    tokenV ">"  Gt
\end{code}
