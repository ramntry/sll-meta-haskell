module MyTreeInterpreter (treeInt) where

import Data
import Data.List
import Data.Maybe
import Examples.Examples
import Driving

treeInt :: Env -> Tree Expr -> Expr

treeInt env (Leaf (Var name _)) = fromJust $ lookup name env
treeInt _ (Leaf value) = value
treeInt env (Node _ (EDecompose cname cargs)) = Ctr cname $ map (treeInt env) cargs
treeInt env (Node _ (ETransient _ next)) = treeInt env next

treeInt env (Node _ (EVariants variants)) = treeInt expandedEnv tree where
  (subst, tree) = fromJust $ find isMatchedWithEnv variants
  isMatchedWithEnv (currentSubst, _) = all varIsMatched currentSubst

  varIsMatched (varName, Ctr cname _)
    | Ctr requestedCName _ <- fromJust (lookup varName env), cname == requestedCName = True
  varIsMatched _ = False

  expandedEnv = foldr addNewBindings env subst
  addNewBindings (varName, Ctr cname args) currEnv =
    zip (map getName args) (getValues currEnv varName) ++ currEnv
  getName (Var name _) = name
  getValues currEnv name | Ctr _ values <- fromJust $ lookup name currEnv = values


natToInt :: Expr -> Int
natToInt (Ctr "Z" []) = 0
natToInt (Ctr "S" [x]) = natToInt x + 1
natToInt _ = error "natToInt: argument is not valid Nat!"

main :: IO ()
main =
  let test  = read "gAdd(gMul(S(S(x)), S(x)), gMul(y, x))" :: Expr
      env   = [("x", read "S(S(Z()))" :: Expr), ("y", read "S(Z())" :: Expr)]
      tree  = buildProcessTree (confMachine counters) test
      value = treeInt env tree
  in
  putStrLn $ "test  = " ++ show test
        ++ "\nenv   = " ++ show env
        ++ "\nvalue = " ++ show value
        ++ " [" ++ show (natToInt value) ++ "]"
