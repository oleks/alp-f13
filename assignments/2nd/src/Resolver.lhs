\begin{code}
module Resolver(
    solve
  , Env
 --   resolve
) where

import Grammar

import Prelude hiding (lookup)
import Data.Map(Map, lookup, insert, empty)
import Data.Foldable(foldlM)

type Env = Map String Value

mgu1 :: Env -> (Value, Value) -> Maybe Env
mgu1 e (Variable xn, Variable yn) =
  case lookup xn e of
    Just _ -> Just e
    Nothing -> case lookup yn e of
                Just _ -> Just e
                Nothing -> Just (insert xn (Variable yn) e)
mgu1 e (Variable xn, v) =
  case lookup xn e of
    Just w -> mgu1 e (w, v)
    Nothing -> Just (insert xn v e)
mgu1 e (v, Variable xn) = mgu1 e (Variable xn, v)
mgu1 e (Symbol x xps, Symbol y yps) =
  if x == y
  then do
    vs <- zipOrFail xps yps
    mgu e vs
  else Nothing

mgu :: Env -> [(Value, Value)] -> Maybe Env
mgu e vs = foldlM mgu1 e vs

zipOrFail :: [a] -> [b] -> Maybe [(a,b)]
zipOrFail (a : as) (b : bs) = do
  tl <- zipOrFail as bs
  return $ (a,b) : tl
zipOrFail [] [] = return []
zipOrFail _ _ = Nothing

rename1 :: (Int, Env) -> Value -> ((Int, Env), Value)
rename1 (i, e) (Variable xn) =
  case lookup xn e of
    Just w -> ((i, e), w)
    Nothing ->
      let i1 = i + 1
          wn = "_G" ++ (show i)
      in ((i1, insert xn (Variable wn) e), Variable wn)
rename1 s (Symbol x xps) =
  let (s1, xps1) = rename s xps
  in  (s1, Symbol x xps1)

rename :: (Int, Env) -> [Value] -> ((Int, Env), [Value])
rename s [] = (s, [])
rename s (v : vs) =
  let (s1, ws) = rename s vs
      (s2, w) = rename1 s1 v
  in  (s2, w : ws)

{-

l_0_0 = Symbol "list" [Symbol "nil" []]
r_0_0 = []
l_1_0 = Symbol "list" [Symbol "cons" [Variable "A", Variable "As"]]
r_1_0 = [Symbol "list" [Variable "As"]]
--g = Symbol "list" [Variable "A"]


l_0 = Symbol "append" [
    Symbol "nil" [],
    Variable "Bs",
    Variable "Bs"
  ]
r_0 = [Symbol "list" [Variable "Bs"]]
l_1 = Symbol "append" [
    Symbol "cons" [Variable "A", Variable "As"],
    Variable "Bs",
    Symbol "cons" [Variable "A", Variable "Cs"]
  ]
r_1 = [Symbol "append" [Variable "As", Variable "Bs", Variable "Cs"]]
g = Symbol "append" [
    Variable "As",
    Symbol "nil" [],
    Symbol "cons" [
      Symbol "1" [],
      Symbol "nil" []
    ]
  ]

((i1_1, e1_1), l1_1) = rename1 (0, empty) l_1
Just e2_1 = mgu1 empty (g, l1_1)

(_, [r1_1]) = rename (i1_1, e1_1) r_1

((i1_2, e1_2), l1_2) = rename1 (i1_1, empty) l_0
Just e2_2 = mgu1 e2_1 (r1_1, l1_2)

(_, [r1_2]) = rename (i1_2, e1_2) r_0

((i1_3, e1_3), l1_3) = rename1 (i1_2, empty) l_1_0
Just e2_3 = mgu1 e2_2 (r1_2, l1_3)

(_, [r1_3]) = rename (i1_3, e1_3) r_1_0

((i1_4, e1_4), l1_4) = rename1 (i1_3, empty) l_1_0
Just e2_4 = mgu1 e2_3 (r1_3, l1_4)

(_, [r1_4]) = rename (i1_4, e1_4) r_1_0

((i1_5, e1_5), l1_5) = rename1 (i1_4, empty) l_0_0
Just e2_5 = mgu1 e2_4 (r1_4, l1_5)

(_, [r1_5]) = rename (i1_5, e1_5) r_0_0

-}

match :: Program -> (Int, Env) -> [Goal] -> [Clause] -> [(Int, Env)]
match _ _ _ [] = []
match _ s [] _ = [s]
match p @ (Program pcs) (i, e) (g:gs) ((Clause l r) : cs) =
  let
    ((i1, e1), l1) = rename1 (i, empty) l
    m1 = case mgu1 e (g, l1) of
            Just e2 ->
              let (_, r1) = rename (i1, e1) r
                  r2 = map (f1 e2) r1
              in match p (i1, e2) (r2 ++ gs) pcs
            Nothing -> []
  in m1 ++ match p (i, e) (g:gs) cs

f1 :: Env -> Value -> Value
f1 e v @ (Variable vn) =
  case lookup vn e of
    Just w -> f1 e w
    Nothing -> v
f1 e (Symbol sn ps) =
  (Symbol sn (map (f1 e) ps))

simplify :: [Value] -> Env -> Env -> Env
simplify [] e0 _ = e0
simplify ((Variable vn) : vs) e0 e =
  let e1 = case lookup vn e of
            Just w -> insert vn (f1 e w) e0
            Nothing -> empty
  in  simplify vs e1 e
simplify ((Symbol _ ps) : vs) e0 e =
  let e1 = simplify ps e0 e
  in  simplify vs e1 e

solve :: Program -> Query -> [Env]
solve p @ (Program pcs) (Query gs) =
  map ((simplify gs empty) . snd) (match p (0, empty) gs pcs)


\end{code}
