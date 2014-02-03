module SmallStepInterpreterCallByValue (int) where

import Data
import Data.Maybe
import DataUtil
import Examples.Examples

int :: Program -> Expr -> (Expr, Int)
int p e = until (isValue . fst) (intStep p) (e, 0)

listIntStep :: Program -> [Expr] -> Int -> Maybe ([Expr], Int)
listIntStep p exprs counter =
  let (values, nonValues) = span isValue exprs in
    case nonValues of
    [] -> Nothing
    (x : xs) -> Just (values ++ (evaledExpr : xs), updatedCounter) where
      (evaledExpr, updatedCounter) = intStep p (x, counter)

intStep :: Program -> (Expr, Int) -> (Expr, Int)

intStep p (Ctr name args, counter) = (Ctr name updatedArgs, updatedCounter) where
  (updatedArgs, updatedCounter) = fromJust $ listIntStep p args counter

intStep p (FCall name args, counter) = case listIntStep p args counter of
  Just (values, updatedCounter) -> (FCall name values, updatedCounter)
  Nothing -> (body // zip vs args, counter + 1) where
    (FDef _ vs body) = fDef p name

intStep p (GCall gname (Ctr cname cargs : args), counter) =
  case listIntStep p cargs counter of
    Just (cValues, updatedCounter) -> (GCall gname (Ctr cname cValues : args), updatedCounter)
    Nothing -> case listIntStep p args counter of
      Just (values, updatedCounter) -> (GCall gname (Ctr cname cargs : values), updatedCounter)
      Nothing -> (body // zip (cvs ++ vs) (cargs ++ args), counter + 1) where
        (GDef _ (Pat _ cvs) vs body) = gDef p gname cname

intStep p (GCall gname (pat : args), counter) =
  (GCall gname (value : args), updatedCounter) where
    (value, updatedCounter) = intStep p (pat, counter)

prettyInt :: Program -> String -> IO ()
prettyInt p exprString =
  let expr = read exprString :: Expr
      (value, counter) = int p expr
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
