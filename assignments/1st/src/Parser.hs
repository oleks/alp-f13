module Parser(
  parse,
  parseFile
) where

import Grammar

import Text.ParserCombinators.ReadP


parse :: String -> Program
parse text =
  case readP_to_S programP text of
    [(p, "")] -> p
    x -> error $ show x

parseFile :: FilePath -> IO Program
parseFile filepath = do
  text <- readFile filepath
  return $ parse text

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

list1P :: ReadP a -> ReadP (List1 a)
list1P p = do
  v <- p
  do { vs <- list1P p; return $ Head v vs } <++ (return $ Limb v)

list1SepByP :: ReadP a -> ReadP sep -> ReadP (List1 a)
list1SepByP p sep = do
  v <- p
  do { _ <- sep; vs <- list1SepByP p sep; return $ Head v vs } <++ (return $ Limb v)

programP :: ReadP Program
programP = do
  fs <- list1P functionP
  return $ Program fs

functionP :: ReadP Function
functionP = do
  h <- headerP
  b <- bodyP
  return $ Function h b

headerP :: ReadP Header
headerP = do
  (fid, ids) <- signatureP
  return $ Header fid ids

signatureP :: ReadP (FunctionId, List1 Id)
signatureP = do
  fid <- functionIdP
  charToken '('
  ids <- list1SepByP idP (charToken ',')
  charToken ')'
  return $ (fid, ids)

bodyP :: ReadP Body
bodyP = do
  charToken '['
  ins <- list1P instructionP
  charToken ']'
  return $ Body ins


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
  (fid, ids) <- signatureP
  return $ AssignCall i fid ids


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

instructionP :: ReadP Instruction
instructionP = labelP <++ assignP <++ storeeP <++ gotoP <++ returnP <++ ifThenElseP

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

numP :: ReadP Int
numP = do
  cs <- stringof ['0'..'9'] ['0'..'9']
  return $ read cs

atomP :: ReadP Atom
atomP =
  do { i <- idP; return $ AtomId i } <++
    do { num <- numP; return $ AtomNum num }

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
  = tokenV "=" Eq   <++
    tokenV "≠" Neq  <++
    tokenV "<" Lt   <++
    tokenV ">" Gt   <++
    tokenV "≤" Leq  <++
    tokenV "≥" Geq
