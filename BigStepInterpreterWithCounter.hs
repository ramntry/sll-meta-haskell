module BigStepInterpreterWithCounter (int) where

import Data
import DataUtil
import Examples.Examples

int :: Program -> Expr -> Int -> (Expr, Int)

listInt :: Program -> [Expr] -> Int -> ([Expr], Int)
listInt p exprs counter = foldr foldStep ([], counter) exprs where
  foldStep expr (ees, cntr) = let (evaledExpr, updatedCounter) = int p expr cntr in
    (evaledExpr : ees, updatedCounter)

int p (Ctr name args) counter = (Ctr name evaledArgs, updatedCounter) where
  (evaledArgs, updatedCounter) = listInt p args counter

int p (FCall name args) counter = int p (body // zip vs args) (counter + 1) where
  (FDef _ vs body) = fDef p name

int p (GCall gname (Ctr cname cargs : args)) counter =
  int p (body // zip (cvs ++ vs) (cargs ++ args)) (counter + 1) where
    (GDef _ (Pat _ cvs) vs body) = gDef p gname cname

int p (GCall gname (pat : args)) counter =
  int p (GCall gname (evaledPat : args)) updatedCounter where
    (evaledPat, updatedCounter) = int p pat counter

prettyInt :: Program -> String -> IO ()
prettyInt p exprString =
  let expr = read exprString :: Expr
      (value, counter) = int p expr 0
      prettyResult = "[" ++ show counter ++ "]: " ++ show value
  in
  putStrLn prettyResult

callByValueMain :: IO ()
callByValueMain = do
  prettyInt progList "gEqNat(S(S(Z())), S(S(Z())))"
  prettyInt callByValueTest "fTest(Z(), S(S(Z())))"
  prettyInt callByValueTest "fTest(fForever(Z()), S(Z()))"

countersMain :: IO ()
countersMain = do
  prettyInt counters "fSecond(gMul(S(S(S(Z()))), S(S(S(S(Z()))))), S(Z()))"
  prettyInt counters "fMakeTetra(gMul(S(S(S(Z()))), S(S(S(S(Z()))))))"
