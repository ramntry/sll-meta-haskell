module BigStepInterpreter (int) where

import Data
import DataUtil
import Examples.Examples

int :: Program -> Expr -> Expr

int p (Ctr name args) = Ctr name (map (int p) args)

int p (FCall name args) = int p (body // zip vs args)
  where (FDef _ vs body) = fDef p name

int p (GCall gname (Ctr cname cargs : args)) =
  int p (body // zip (cvs ++ vs) (cargs ++ args)) where
    (GDef _ (Pat _ cvs) vs body) = gDef p gname cname

int p (GCall gname (pat : args)) =
  int p (GCall gname (int p pat : args))
