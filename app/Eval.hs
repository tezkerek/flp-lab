module Eval where

import           Data.List                      ( delete
                                                , nub
                                                , union
                                                )
import           Debug.Trace                    ( trace )
import           Exp

vars :: Exp -> [IndexedVar]
vars ex = case ex of
  X iv      -> [iv]
  Lam iv e  -> nub $ iv : vars e
  App e1 e2 -> vars e1 `union` vars e2

-- >>> vars (Lam (makeIndexedVar "x") (X (makeIndexedVar "y")))
-- [IndexedVar {ivName = "x", ivCount = 0},IndexedVar {ivName = "y", ivCount = 0}]

-- >>> vars (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "y", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0})))
-- [IndexedVar {ivName = "x", ivCount = 0},IndexedVar {ivName = "y", ivCount = 0},IndexedVar {ivName = "z", ivCount = 0}]

-- >>> vars (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0}))) (X (IndexedVar {ivName = "x", ivCount = 0})))
-- [IndexedVar {ivName = "x", ivCount = 0}]

freeVars :: Exp -> [IndexedVar]
freeVars ex = case ex of
  X iv      -> [iv]
  Lam iv e  -> delete iv $ freeVars e
  App e1 e2 -> freeVars e1 `union` freeVars e2

-- >>> freeVars (Lam (makeIndexedVar "x") (X (makeIndexedVar "y")))
-- [IndexedVar {ivName = "y", ivCount = 0}]

-- >>> freeVars  (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "y", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0})))
-- [IndexedVar {ivName = "y", ivCount = 0},IndexedVar {ivName = "z", ivCount = 0}]

-- >>> freeVars (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0}))) (X (IndexedVar {ivName = "x", ivCount = 0})))
-- [IndexedVar {ivName = "x", ivCount = 0}]

-- >>> freeVars (Lam (IndexedVar {ivName = "x", ivCount = 0}) (App (X (IndexedVar {ivName = "x", ivCount = 0})) (X (IndexedVar {ivName = "x", ivCount = 0}))))
-- []

occursFree :: IndexedVar -> Exp -> Bool
occursFree iv ex = iv `elem` freeVars ex

-- >>> makeIndexedVar "x" `occursFree` Lam (makeIndexedVar "x") (X (makeIndexedVar "y"))
-- False

-- >>> makeIndexedVar "y" `occursFree` Lam (makeIndexedVar "x") (X (makeIndexedVar "y"))
-- True

freshVar :: IndexedVar -> [IndexedVar] -> IndexedVar
freshVar v ivs =
  let ivs'     = filter ((ivName v ==) . ivName) ivs
      maxIndex = foldr (max . ivCount) (-1) ivs'
  in  IndexedVar (ivName v) (maxIndex + 1)

-- >>> freshVar (makeIndexedVar "x") [makeIndexedVar "x"]
-- IndexedVar {ivName = "x", ivCount = 1}

-- >>> freshVar (makeIndexedVar "x") [makeIndexedVar "x", IndexedVar {ivName = "x", ivCount = 1}, IndexedVar {ivName = "y", ivCount = 2}] 
-- IndexedVar {ivName = "x", ivCount = 2}

renameVar :: IndexedVar -> IndexedVar -> Exp -> Exp
renameVar toReplace replacement ex =
  let nextRename = renameVar toReplace replacement
  in  case ex of
        X iv -> if iv == toReplace then X replacement else ex
        Lam iv e ->
          if iv == toReplace then Lam replacement (nextRename e) else ex
        App e1 e2 -> App (nextRename e1) (nextRename e2)

-- >>> renameVar (IndexedVar {ivName = "x", ivCount = 0}) (IndexedVar {ivName = "z", ivCount = 0}) (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0}))) (X (IndexedVar {ivName = "x", ivCount = 0})))
-- App (Lam (IndexedVar {ivName = "z", ivCount = 0}) (X (IndexedVar {ivName = "z", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0}))

substitute :: IndexedVar -> Exp -> Exp -> Exp
substitute toReplace replacement ex =
  let nextSubstitute = substitute toReplace replacement
  in  case ex of
        X iv           -> if iv == toReplace then replacement else ex
        App e1 e2      -> App (nextSubstitute e1) (nextSubstitute e2)
        Lam iv lamBody -> if iv == toReplace
          then ex
          else if iv `occursFree` replacement
            then
              let newVar  = freshVar iv (vars lamBody `union` vars replacement)
                  newBody = renameVar iv newVar lamBody
                  newLam  = Lam newVar (nextSubstitute newBody)
              in  trace (show (vars lamBody) ++ " AAA " ++ show newVar) newLam
            else Lam iv (nextSubstitute lamBody)

testSubstitute =
  substitute
      (IndexedVar "y" 0)
      (X (IndexedVar "x" 0))
      (App (Lam (IndexedVar "x" 0) (X (IndexedVar "y" 0)))
           (X (IndexedVar "z" 0))
      )
    == App
         (Lam (IndexedVar "x" 1) (X (IndexedVar "x" 0)))
         (X (IndexedVar "z" 0))

-- >>> substitute (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "z", ivCount = 0})) (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0}))) (X (IndexedVar {ivName = "x", ivCount = 0})))
-- App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0}))

-- >>> substitute (IndexedVar {ivName = "y", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0})) (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "y", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0})))
-- App (Lam (IndexedVar {ivName = "x", ivCount = 1}) (X (IndexedVar {ivName = "x", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0}))

normalize :: Exp -> Exp
normalize ex = maybe ex normalize (step ex)
 where
  step (X _                    ) = Nothing
  step (Lam x               e  ) = Lam x <$> step e
  step (App (Lam x lamBody) arg) = Just (substitute x arg lamBody)
  step (App e1              e2 ) = case step e1 of
    Nothing  -> App e1 <$> step e2
    Just e1' -> Just $ App e1' e2

-- >>> normalize (X (makeIndexedVar "x"))
-- X (IndexedVar {ivName = "x", ivCount = 0})

-- >>> normalize (App (Lam (IndexedVar {ivName = "y", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0}))) (App (Lam (IndexedVar {ivName = "y", ivCount = 0}) (App (X (IndexedVar {ivName = "y", ivCount = 0})) (X (IndexedVar {ivName = "y", ivCount = 0})))) (Lam (IndexedVar {ivName = "y", ivCount = 0}) (App (X (IndexedVar {ivName = "y", ivCount = 0})) (X (IndexedVar {ivName = "y", ivCount = 0}))))))
-- X (IndexedVar {ivName = "x", ivCount = 0})
