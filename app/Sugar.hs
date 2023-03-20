module Sugar where

import           Exp

desugarVar :: Var -> IndexedVar
desugarVar Var { getVar = v } = makeIndexedVar v

-- >>> desugarVar (Var "x")
-- IndexedVar {ivName = "x", ivCount = 0}

sugarVar :: IndexedVar -> Var
sugarVar IndexedVar { ivName = name, ivCount = count } =
  Var $ name ++ case count of
    0 -> ""
    _ -> "_" ++ show count

-- >>> sugarVar (IndexedVar "x" 0)
-- Var {getVar = "x"}

-- >>> sugarVar (IndexedVar "x" 3)
-- Var {getVar = "x_3"}

consExp, nilExp, zeroExp, succExp, fixExp :: Exp
consExp = X (makeIndexedVar ":")  -- : :: a -> List a -> List a  list constructor
nilExp = X (makeIndexedVar "Nil") -- Nil :: List a               empty list
zeroExp = X (makeIndexedVar "Z")  -- Z :: Natural                zero
succExp = X (makeIndexedVar "S")  -- S :: Natural -> Natural     successor
fixExp = X (makeIndexedVar "fix") -- fix :: (a -> a) -> a        fixpoint fn.

desugarExp :: ComplexExp -> Exp
desugarExp cx = case cx of
  CX cv -> X v where v = desugarVar cv
  CLam cv ce ->
    let v = desugarVar cv
        e = desugarExp ce
    in  Lam v e
  CApp ce1 ce2 ->
    let e1 = desugarExp ce1
        e2 = desugarExp ce2
    in  App e1 e2
  List cexps ->
    foldr (\ce s -> App (App consExp (desugarExp ce)) s) nilExp cexps
  Nat n -> foldr (\_ s -> App succExp s) zeroExp [1 .. n]
  Let cv ce1 ce2 ->
    let v  = desugarVar cv
        e1 = desugarExp ce1
        e2 = desugarExp ce2
    in  App (Lam v e1) e2
  LetRec cv ce1 ce2 ->
    let v  = desugarVar cv
        e1 = desugarExp ce1
        e2 = desugarExp ce2
    in  App (Lam v e2) (App fixExp (Lam v e1))

-- >>> desugarExp (CApp (CLam (Var "x") (CX (Var "y"))) (CX (Var "z"))) 
-- App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "y", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0}))

-- >>> desugarExp (Nat 3)
-- App (X (IndexedVar {ivName = "S", ivCount = 0})) (App (X (IndexedVar {ivName = "S", ivCount = 0})) (App (X (IndexedVar {ivName = "S", ivCount = 0})) (X (IndexedVar {ivName = "Z", ivCount = 0}))))

-- >>> desugarExp (List [CX (Var "y"), CX (Var "x")])
-- App (App (X (IndexedVar {ivName = ":", ivCount = 0})) (X (IndexedVar {ivName = "y", ivCount = 0}))) (App (App (X (IndexedVar {ivName = ":", ivCount = 0})) (X (IndexedVar {ivName = "x", ivCount = 0}))) (X (IndexedVar {ivName = "Nil", ivCount = 0})))

-- >>> desugarExp (Let (Var "y") (CX (Var "x")) (CX (Var "z")))
-- App (Lam (IndexedVar {ivName = "y", ivCount = 0}) (X (IndexedVar {ivName = "z", ivCount = 0}))) (X (IndexedVar {ivName = "x", ivCount = 0}))

-- >>> desugarExp (LetRec (Var "y") (CX (Var "x")) (CX (Var "z")))
-- App (Lam (IndexedVar {ivName = "y", ivCount = 0}) (X (IndexedVar {ivName = "z", ivCount = 0}))) (App (X (IndexedVar {ivName = "fix", ivCount = 0})) (Lam (IndexedVar {ivName = "y", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0}))))

sugarExp :: Exp -> ComplexExp
sugarExp ex = case ex of
  X iv -> let cv = sugarVar iv in CX cv
  Lam iv e ->
    let v  = sugarVar iv
        ce = sugarExp e
    in  CLam v ce
  App e1 e2 ->
    let ce1 = sugarExp e1
        ce2 = sugarExp e2
    in  CApp ce1 ce2

-- >>> sugarExp (App (X (IndexedVar "x" 0)) (X (IndexedVar "y" 1)))
-- CApp (CX (Var {getVar = "x"})) (CX (Var {getVar = "y_1"}))

-- >>> sugarExp (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "y", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0})))
-- (CApp (CLam (Var "x") (CX (Var "y"))) (CX (Var "z"))) 
