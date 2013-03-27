\begin{code}
module Resolution(
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

{-
match :: Goal -> [Goal] -> Env -> [Clause] -> [Substitution]
match g gs env [] = undefined
-}
{-
renameUnify :: Value -> Env -> Env
renameUnify (Value x []) =
-}

mgu1 :: [(Value, Value)] -> Maybe Env
mgu1 vs = mgu empty vs

mgu :: Env -> [(Value, Value)] -> Maybe Env
mgu e [] = Just e
mgu e ((Variable _, Variable _) : ps) =
  mgu e ps
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

match :: Program -> (Int, Env) -> [Goal] -> [Clause] -> [(Int, Env)]
match _ _ _ [] = []
match _ e [] _ = [e]
match p @ (Program pcs) (i, e) (g:gs) ((Clause l r) : cs) =
  let
    ((i1, e1), l1) = rename1 ((i, e), l)
    m1 = case mgu e1 [(g, l1)] of
            Just e2 ->
              let ((i2, _), r1) = rename ((i1, e2), r)
              in match p (i2, e2) (r1 ++ gs) pcs
            Nothing -> []
  in m1 ++ (match p (i, e) (g:gs) cs)

rename1 :: ((Int, Env), Value) -> ((Int, Env), Value)
rename1 ((i, e), (Variable vn)) =
  case Map.lookup vn e of
    Just w -> ((i, e), w)
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


l1 = Symbol "h" [Variable "X"]
r1 = []--[Symbol "g" [Variable "X"]]
g = Symbol "h" [Variable "X"]
((i11, e11), l11) = rename1 ((0, empty), l1)
Just e12 = mgu e11 [(g, l11)]
((i12, _), r11) = rename ((i11, e12), r1)

--pcs = [Clause l1 []]
--p = Program pcs

--Just e1 = mgu empty [(g, l2)]

--m1 = match p empty u1 pcs

--mt = mgu empty [(Variable "A", Symbol "cons" [Symbol "5" []])]


{-
l1 = Symbol "list" [Symbol "nil" []]
l2 = Symbol "list" [Symbol "cons" [Variable "A", Variable "As"]]
r2 = [Symbol "list" [Variable "As"]]
g = Symbol "list" [
      Symbol "cons" [
        Symbol "5" [],
        Symbol "cons" [
          Symbol "6"[],
          Symbol "nil" []
        ]
      ]
    ]

pcs = [Clause l1 [], Clause l2 r2]
p = Program pcs

Just e1 = mgu empty [(g, l2)]
u1 = unify e1 r2

m1 = match p empty u1 pcs

mt = mgu empty [(Variable "A", Symbol "cons" [Symbol "5" []])]


--(e1,l1) = rename1 (empty, l)
-}

\end{code}
