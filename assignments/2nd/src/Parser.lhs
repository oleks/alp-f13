\section{Parser}\label{section:parser}

The parser is implemented by transforming the Pl grammar into a parsing
expression grammar, and implementing the parser using the \texttt{ReadP} parser
combinator. The code is relatively straight-forward once in is informed that
the \texttt{(<++)} operator is a choice operator that only considers the parser
to the right if the parser to the left fails.

\begin{code}
module Parser(
  parseModule,
  parseQuery,
  parseModuleFile
) where
\end{code}

\ignore{
\begin{code}
import Grammar

import Text.ParserCombinators.ReadP
import Control.Monad(void)
\end{code}
}

\subsection{Exported functions}

The user can either parse a module or a query specified by a given string or
parse a stored at a given file path. If any of these fails, the program dies.

\begin{code}
parseModule :: String -> Module
parseModule text =
  case readP_to_S moduleP text of
    [(p, "")] -> p
    x -> error $ show x

parseQuery :: String -> Query
parseQuery text =
  case readP_to_S queryP text of
    [(p, "")] -> p
    x -> error $ show x

parseModuleFile :: FilePath -> IO Module
parseModuleFile filepath = do
  text <- readFile filepath
  return $ parseModule text
\end{code}

\subsection{Auxiliary parser combinators}

A couple generically useful parser combinators, extending the functionality of
\texttt{ReadP}.

\begin{code}
token :: ReadP a -> ReadP a
token p = do
  skipSpaces
  v <- p
  skipSpaces
  return v

charToken :: Char -> ReadP ()
charToken c = void $ token $ char c

stringToken :: String -> ReadP ()
stringToken s = void $ token $ string s

charof :: [Char] -> ReadP Char
charof cs = satisfy $ \c -> elem c cs

-- vcs': Valid first characters; vcs: valid tailing characters.
stringof :: [Char] -> [Char] -> ReadP String
stringof vcs' vcs = token $ do
    c <- charof vcs'
    cs <- many $ charof vcs
    return $ c : cs
\end{code}

\subsection{Pl}

A module consists of 0 or more clauses:

\begin{code}
moduleP :: ReadP Module
moduleP = do
  cs <- many clauseP
  eof
  return $ Module cs
\end{code}

A clause is a value followed by a turnstile and 0 or more values separated by
commas, terminated by full stop:

\begin{code}
clauseP :: ReadP Clause
clauseP = do
  l <- valueP
  rs <- do {
      stringToken ":-";
      sepBy valueP (charToken ',')
    } <++ return []
  charToken '.'
  return $ Clause l rs
\end{code}

\subsubsection{Values}

A value is either a variable, a symbol, a list, a value in parentheses (added
to allow to override the right-associativity of equality), or a cut. A value
may also be a comparison of a pair of values, denoted using an infix
right-associative, lowest precedence operator \texttt{=}.

\begin{code}
valueP :: ReadP Value
valueP = do
  v1 <- variableP <++ symbolP <++ listP <++ parenValueP <++ cutP
  do {
    charToken '=';
    v2 <- valueP;
    return $ Eq v1 v2
  } <++ return v1

parenValueP :: ReadP Value
parenValueP = do
  charToken '('
  v <- valueP
  charToken ')'
  return v
\end{code}

\subsubsection{Variables and symbols}

Both variables and symbols may end in a sequence of characters that are
alphanumeric or \texttt{\_}. Variables however must start with a capital
alphabetic character, while symbols must start with a lower-cased alphabetic
character or a digit. Symbols may additionally have a number of arguments in
parentheses.

\begin{code}
tailChars :: [Char]
tailChars = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_"

variableP :: ReadP Value
variableP = do
  name <- stringof ['A'..'Z'] tailChars
  return $ Variable name

symbolP :: ReadP Value
symbolP = do
  name <- stringof (['a'..'z'] ++ ['0'..'9']) tailChars
  ps <- parametersP <++ return []
  return $ Symbol name ps

parametersP :: ReadP [Value]
parametersP = do
  charToken '('
  vs <- sepBy valueP (charToken ',')
  charToken ')'
  return vs
\end{code}


\subsubsection{Lists}

As mentioned in \referToSection{grammar}, Pl has list notation. This notation
is transformed into Pure Prolog notation in a straight-forward manner:

\begin{code}
listP :: ReadP Value
listP = do
  charToken '['
  l <- list1P <++ nil
  charToken ']'
  return l

list1P :: ReadP Value
list1P = do
  v <- valueP;
  vs <- do { charToken ','; list1P; } <++
      do { charToken '|'; variableP } <++
      nil
  return $ Symbol "_cons" [v, vs]

nil :: ReadP Value
nil = return $ Symbol "_nil" []
\end{code}

\subsubsection{Cuts}

A cut is merely the symbol \texttt{!}.

\begin{code}
cutP :: ReadP Value
cutP = do
  charToken '!'
  return Cut
\end{code}

\subsection{Queries}

A query is a symbol \texttt{?-} followed by a comma-separated list of values,
terminated by a full stop:

\begin{code}
queryP :: ReadP Query
queryP = do
  stringToken "?-"
  ps <- sepBy valueP (charToken ',')
  charToken '.'
  eof
  return $ Query ps

\end{code} 
