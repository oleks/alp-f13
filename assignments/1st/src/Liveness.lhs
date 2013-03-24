module Analysis(
  analyzeFile
) where

import Grammar
import Parser

import Data.Map(Map,(!), insert)
import qualified Data.Map as Map
import Data.Array(Array, bounds, assocs)
import Data.Set(Set, empty, singleton, fromList, unions, union, difference)

type LabelMap = Map LabelId Int
type OutSet = Map Int (Set Id)
type InSet = Map Int (Set Id)
type OutInSets = (OutSet, InSet)

analyzeFile :: FilePath -> IO ()
analyzeFile path = do
  program <- parseFile path
  mapM_ (\s -> putStrLn $ show s ++ "\n\n") (programL program)

programL :: Program -> [OutInSets]
programL (Program lfs) = do
  let fs = list1list lfs
  map functionL fs

functionL :: Function -> OutInSets
functionL (Function _ b) = bodyL b

bodyL :: Body -> OutInSets
bodyL (Body a m) = do
  let revIns = reverse $ assocs a
  fixedPointIter revIns m (emptyOutInSets a)

emptyOutInSets :: Array Int Instruction -> OutInSets
emptyOutInSets a =
  let
    (minI, maxI) = bounds a
    emptyIdSet = Map.fromList [ (i, empty) | i <- [minI..maxI] ]
  in
    (emptyIdSet, emptyIdSet)

fixedPointIter :: [(Int, Instruction)] -> LabelMap -> OutInSets -> OutInSets
fixedPointIter a m s =
  let s1 = foldl (iter m) s a
  in  if s == s1
      then s
      else fixedPointIter a m s1

iter :: LabelMap -> OutInSets -> (Int, Instruction) -> OutInSets
iter m (outSet, inSet) x @ (i, _) =
  let
    outSet' = insert i (nextOutSet m inSet x) outSet
    inSet' = insert i (nextInSet outSet x) inSet
  in
    (outSet', inSet')

nextOutSet :: LabelMap -> InSet -> (Int, Instruction) -> Set Id
nextOutSet m inSet x =
  unions $ map (inSet !) $ succ' m x

nextInSet :: OutSet -> (Int, Instruction) -> Set Id
nextInSet outSet (i, ins) =
  union (gen ins) (difference (outSet ! i) (kill ins))

gen :: Instruction -> Set Id
gen (Assign _ (AtomId y))           = fromList [y]
gen (AssignUnop _ _ (AtomId y))     = fromList [y]
gen (AssignBinop _ y _ (AtomId z))  = fromList [y, z]
gen (AssignBinop _ y _ _)           = fromList [y]
gen (Load _ (AtomId y))             = fromList [y]
gen (Store (AtomId x) y)            = fromList [x, y]
gen (Store _ y)                     = fromList [y]
gen (IfThenElse x _ (AtomId y) _ _) = fromList [x, y]
gen (AssignCall _ _ (Args args))    = fromList (list1list args)
gen (Return x)                      = fromList [x]
gen _                               = empty

kill :: Instruction -> Set Id
kill (Assign x _)                   = singleton x
kill (AssignUnop x _ _)             = singleton x
kill (AssignBinop x _ _ _)          = singleton x
kill (Load x _)                     = singleton x
kill (AssignCall x _ _)             = singleton x
kill _                              = empty

{-

Only works if the last instruction is guaranteed to be a GOTO, an --
IF-THEN-ELSE or a RETURN. The return type is list as the set will merely be
traversed.

-}

succ' :: Map LabelId Int -> (Int, Instruction) -> [Int]
succ' m (i, ins) =
  case ins of
    Goto l                  -> [m ! l]
    IfThenElse _ _ _ l1 l2  -> [m ! l1, m ! l2]
    Return _                -> []
    _                       -> [i + 1]
