\begin{code}
module Resolver(
    resolve
  , Env
  , mgu
  , mgu1
  , solve
) where

import Grammar

import Data.Map(Map, empty, insert, union)
import qualified Data.Map as Map

type Env = Map String Value

resolve :: Program -> Query -> [Substitution]
resolve (Program _) (Query _) = undefined

mgu1 :: [(Value, Value)] -> Maybe Env
mgu1 vs = mgu empty vs

mgu :: Env -> [(Value, Value)] -> Maybe Env
mgu e [] = Just e
mgu e ((Variable xn, Variable yn) : ps) =
  if xn == yn
  then mgu e ps
  else mgu (insert xn (Variable yn) e) ps
mgu e ((Variable xn, v @ (Symbol _ _)) : ps) =
  case Map.lookup xn e of
    Just w  -> mgu e ((w, v) : ps)
    Nothing -> mgu (insert xn v e) ps
mgu e ((v @ (Symbol _ _), x @ (Variable _)) : ps) =
  mgu e ((x, v) : ps)
mgu e (((Symbol x xps), (Symbol y yps)) : ps) =
  if x == y
  then do
    vs <- zipOrFail xps yps
    mgu e (vs ++ ps)
  else Nothing

zipOrFail :: [a] -> [b] -> Maybe [(a,b)]
zipOrFail (a : as) (b : bs) = do
  tl <- zipOrFail as bs
  return $ (a,b) : tl
zipOrFail [] [] = return []
zipOrFail _ _ = Nothing


rename1 :: ((Int, Env), Value) -> ((Int, Env), Value)
rename1 ((i, e), (Variable vn)) =
  case Map.lookup vn e of
    Just w -> ((i, e), f1 e w)
    Nothing ->
        let w = (Variable ("_G" ++ (show i)))
        in ((i + 1, insert vn w e), w)
rename1 ((i, e), (Symbol sn ps)) =
  let ((i1, e1), ps1) = rename ((i, e), ps)
  in  ((i1, e1), Symbol sn ps1)

rename :: ((Int, Env), [Value]) -> ((Int, Env), [Value])
rename (i, []) = (i, [])
rename (i, (v : vs)) =
  let (j, w) = rename1 (i, v)
      (k, ws) = rename (j, vs)
  in  (k, w : ws)

f1 :: Env -> Value -> Value
f1 e v @ (Variable vn) =
  case Map.lookup vn e of
    Just w -> f1 e w
    Nothing -> v
f1 e (Symbol sn ps) =
  (Symbol sn (map (f1 e) ps))

simplify :: [Value] -> Env -> Env -> Env
simplify [] e0 _ = e0
simplify (v @ (Variable vn) : vs) e0 e =
  let e1 = case Map.lookup vn e of
            Just w -> insert vn (f1 e w) e0
            Nothing -> empty
  in  simplify vs e1 e
simplify (v @ (Symbol sn ps) : vs) e0 e =
  let e1 = simplify ps e0 e
  in  simplify vs e1 e

solve :: Program -> Query -> [Env]
solve p @ (Program pcs) (Query gs) =
  map ((simplify gs empty) . snd) (match p (0, empty) gs pcs)

match :: Program -> (Int, Env) -> [Goal] -> [Clause] -> [(Int, Env)]
match _ _ _ [] = []
match _ e [] _ = [e]
match p @ (Program pcs) (i, e) (g:gs) ((Clause l r) : cs) =
  let
    ((i1, e1), l1) = rename1 ((i, e), l)
    m1 = case mgu e1 [(g, l1)] of
            Just e2 ->
              let ((i2, _), r1) = rename ((i1, e2), r)
              in match p (i2, e1) (r1 ++ gs) pcs
            Nothing -> []
  in m1 ++ (match p (i, e) (g:gs) cs)

l1 = Symbol "list" [Symbol "nil" []]
l2 = Symbol "list" [Symbol "cons" [Variable "A", Variable "As"]]
r2 = [Symbol "list" [Variable "As"]]
g = Symbol "list" [Variable "A"]
{- g = Symbol "list" [
      Symbol "cons" [
        Symbol "5" [],
        Symbol "cons" [
          Symbol "6"[],
          Symbol "nil" []
        ]
      ]
    ]-}

pcs = [Clause l1 [], Clause l2 r2]
p = Program pcs

((i1_1, e1_1), l1_1) = rename1 ((0, empty), l1)
u1 = mgu e1_1 [(g,l1_1)]

((i1_2, e1_2), l1_2) = rename1 ((0, empty), l2)
Just e2_2 = mgu e1_2 [(g,l1_2)]

((i2_2, _), [r1_2]) = rename ((i1_2, e2_2), r2)

((i1_3, e1_3), l1_3) = rename1 ((i2_2, e1_2), l2)
Just e2_3 = mgu e1_3 [(r1_2,l1_3)]

((i2_3, _), [r1_3]) = rename ((i1_3, e2_3), r2)

((i1_4, e1_4), l1_4) = rename1 ((i2_3, e1_3), l1)
Just e2_4 = mgu e1_4 [(r1_3,l1_4)]

--Just e3_4 = mgu e1_2 [(r2_2,l1_2)]

--match p (i2, e2) (r1 ++ gs) pcs


--m1 = match p empty u1 pcs

--mt = mgu empty [(Variable "A", Symbol "cons" [Symbol "5" []])]


\end{code}
