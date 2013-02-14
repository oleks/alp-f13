module Analysis where

import Grammar

class Gen a where
  gen :: a -> [Id]

class Kill a where
  kill :: a -> [Id]

instance Gen Instruction where
  gen (Assign _ (AtomId y))           = [y]
  gen (AssignUnop _ _ (AtomId y))     = [y]
  gen (AssignBinop _ y _ (AtomId z))  = [y, z]
  gen (AssignBinop _ y _ _)           = [y]
  gen (Load _ (AtomId y))             = [y]
  gen (Store (AtomId x) y)            = [x, y]
  gen (Store _ y)                     = [y]
  gen (IfThenElse x _ (AtomId y) _ _) = [x, y]
  gen (AssignCall _ _ args)           = list1list args
  gen (Return x)                      = [x]
  gen _                               = []

instance Kill Instruction where
  kill (Assign x _)                   = [x]
  kill (AssignUnop x _ _)             = [x]
  kill (AssignBinop x _ _ _)          = [x]
  kill (Load x _)                     = [x]
  kill (AssignCall x _ _)             = [x]
  kill _                              = []
