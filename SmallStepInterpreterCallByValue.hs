module SmallStepInterpreterCallByValue (int) where

import Data
import DataUtil
import Examples.Examples

int :: Program -> Expr -> (Expr, Int)
int p e = until (isValue . fst) (intStep p) (e, 0)

intStep :: Program -> (Expr, Int) -> (Expr, Int)

intStep p (Ctr name args, counter) =
  (Ctr name (values ++ (evaledArg : xs)), updatedCounter) where
    (values, x : xs) = span isValue args
    (evaledArg, updatedCounter) = intStep p (x, counter)

intStep p (FCall name args, counter) =
  let (values, nonValues) = span isValue args
      (FDef _ vs body) = fDef p name
  in
  case nonValues of
    [] -> (body // zip vs values, counter + 1)
    (x : xs) -> (FCall name (values ++ (evaledArg : xs)), updatedCounter) where
      (evaledArg, updatedCounter) = intStep p (x, counter)

{-
intStep p (GCall gname (Ctr cname cargs : args)) =
  body // zip (cvs ++ vs) (cargs ++ args) where
    (GDef _ (Pat _ cvs) vs body) = gDef p gname cname

intStep p (GCall gname (e:es)) =
  (GCall gname (intStep p e : es))
-}

prettyInt :: Program -> String -> IO ()
prettyInt p exprString =
  let expr = read exprString :: Expr
      (value, counter) = int p expr
      prettyResult = "[" ++ show counter ++ "]: " ++ show value
  in
  putStrLn prettyResult

callByValueMain :: IO ()
callByValueMain = do
  prettyInt callByValueTest "fTest(Z(), S(S(Z())))"
  prettyInt callByValueTest "fTest(fForever(Z()), S(Z()))"
