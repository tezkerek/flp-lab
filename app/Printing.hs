module Printing
  ( showExp
  ) where

import           Data.List                      ( intercalate )
import           Exp

showVar :: Var -> String
showVar = getVar

showExp :: ComplexExp -> String
showExp (CX  v       ) = showVar v
showExp (Nat n       ) = show n
showExp (CLam v  cexp) = "(\\" ++ showVar v ++ " -> " ++ showExp cexp ++ ")"
showExp (CApp e1 e2  ) = "(" ++ showExp e1 ++ " " ++ showExp e2 ++ ")"
showExp (Let v e1 e2) =
  "(let " ++ showVar v ++ " := " ++ showExp e1 ++ " in " ++ showExp e2 ++ ")"
showExp (LetRec v e1 e2) =
  "(letrec " ++ showVar v ++ " := " ++ showExp e1 ++ " in " ++ showExp e2 ++ ")"
showExp (List l) = "[" ++ intercalate ", " (map showExp l) ++ "]"
