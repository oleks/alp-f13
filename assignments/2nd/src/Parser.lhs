\section{Parser}

The parser is implemented by transforming the grammar defined in \cite[Section
3.2 (p.  36)]{torben} into a parsing expression grammar, and implementing the
parser using the \texttt{ReadP} parser combinator. The code is relatively
straight-forward once in is informed that the \texttt{(<++)} operator is a
choice operator that only considers the parser to the right if the parser to
the left fails.

\begin{code}
module Parser(
  parseProgram,
  parseQuery,
  parseProgramFile
) where
\end{code}

\ignore{
\begin{code}
import Grammar
\end{code}

\begin{code}
import Text.ParserCombinators.ReadP
\end{code}
}

\subsection{Exported functions}

The user can either parse a program in a string or a program stored at a given
path.

\begin{code}
parseProgram :: String -> Program
parseProgram text =
  case readP_to_S programP text of
    [(p, "")] -> p
    x -> error $ show x

parseQuery :: String -> Query
parseQuery text =
  case readP_to_S queryP text of
    [(p, "")] -> p
    x -> error $ show x

parseProgramFile :: FilePath -> IO Program
parseProgramFile filepath = do
  text <- readFile filepath
  return $ parseProgram text
\end{code}

\subsection{Auxiliary parser combinators}

A couple generically useful parser combinators, extending the functionality of
\texttt{ReadP}.

\begin{code}
skip :: ReadP a -> ReadP ()
skip p = do
  _ <- p
  return ()

token :: ReadP a -> ReadP a
token p = do
  skipSpaces
  v <- p
  skipSpaces
  return v

charToken :: Char -> ReadP ()
charToken c = skip $ token $ char c

stringToken :: String -> ReadP ()
stringToken s = skip $ token $ string s

charof :: [Char] -> ReadP Char
charof cs = satisfy $ \c -> elem c cs

stringof :: [Char] -> [Char] -> ReadP String
stringof cs1 cs2 = token $ do
    c <- charof cs1
    cs <- many $ charof cs2
    return $ c : cs
\end{code}

\begin{code}
programP :: ReadP Program
programP = do
  cs <- many clauseP
  eof
  return $ Program cs

clauseP :: ReadP Clause
clauseP = do
  l <- valueP
  rs <- do { stringToken ":-"; sepBy valueP (charToken ',') } <++ return []
  charToken '.'
  return $ Clause l rs

valueP :: ReadP Value
valueP = variableP <++ functionSymbolP

tailChars :: [Char]
tailChars = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_"

variableP :: ReadP Value
variableP = do
  name <- stringof ['A'..'Z'] tailChars
  return $ Value name []

functionSymbolP :: ReadP Value
functionSymbolP = do
  name <- stringof (['a'..'z'] ++ ['0'..'9']) tailChars
  ps <- parametersP <++ return []
  return $ Value name ps

parametersP :: ReadP [Value]
parametersP = do
  charToken '('
  vs <- sepBy valueP (charToken ',')
  charToken ')'
  return vs

queryP :: ReadP Query
queryP = do
  stringToken "?-"
  ps <- sepBy valueP (charToken ',')
  charToken '.'
  eof
  return $ Query ps

\end{code} 
