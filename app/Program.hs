module Program where
import           Eval                           ( substitute )
import           Exp
-- import           Lab2                           ( Parser
--                                                 , endOfInput
--                                                 , reserved
--                                                 , semiSep1
--                                                 , whiteSpace
--                                                 )
import           Parsing                        ( expr
                                                , miniHs
                                                , var
                                                )
import           Sugar                          ( desugarExp
                                                , desugarVar
                                                )

-- import           Control.Applicative            ( Alternative(..) )
import qualified Data.Map.Strict               as Map
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Token

data Definition = Definition
  { defHead :: Var
  , defArgs :: [Var]
  , defBody :: ComplexExp
  }
  deriving Show

definition :: Parser Definition
definition = do
  f  <- var
  ps <- many var
  reservedOp miniHs ":="
  ex <- expr
  return $ Definition f ps ex

-- >>> parseFirst definition "id := \\x -> x"
-- Just (Definition {defHead = Var {getVar = "id"}, defArgs = [], defBody = CLam (Var {getVar = "x"}) (CX (Var {getVar = "x"}))})

-- >>> parseFirst definition "id x := x"
-- Just (Definition {defHead = Var {getVar = "id"}, defArgs = [Var {getVar = "x"}], defBody = CX (Var {getVar = "x"})})

-- >>> parseFirst definition "const x y := x"
-- Just (Definition {defHead = Var {getVar = "const"}, defArgs = [Var {getVar = "x"},Var {getVar = "y"}], defBody = CX (Var {getVar = "x"})})

program :: Parser [Definition]
program = whiteSpace miniHs *> many command <* eof
 where
  command = do
    d <- definition
    reservedOp miniHs ";"
    return d

-- >>> parseFirst program "    id x := x ; const x y := x"
-- Nothing

-- >>> parseFirst program "    id x := x ; const x y := x ;"
-- Just [Definition {defHead = Var {getVar = "id"}, defArgs = [Var {getVar = "x"}], defBody = CX (Var {getVar = "x"})},Definition {defHead = Var {getVar = "const"}, defArgs = [Var {getVar = "x"},Var {getVar = "y"}], defBody = CX (Var {getVar = "x"})}]

definitionExp :: Definition -> ComplexExp
definitionExp def =
  let args = defArgs def
      body = defBody def
  in  foldr CLam body args

-- >>> definitionExp (Definition {defHead = Var {getVar = "const"}, defArgs = [Var {getVar = "x"},Var {getVar = "y"}], defBody = CX (Var {getVar = "x"})})
-- CLam (Var {getVar = "x"}) (CLam (Var {getVar = "y"}) (CX (Var {getVar = "x"})))

type Environment = Map.Map IndexedVar Exp

programEnv :: [Definition] -> Environment
programEnv pgm =
  let toBinding :: Definition -> (IndexedVar, Exp)
      toBinding def =
        (desugarVar $ defHead def, desugarExp (definitionExp def))
  in  Map.fromList $ map toBinding pgm

normalizeEnv :: Environment -> Exp -> Exp
normalizeEnv env ex = maybe ex (normalizeEnv env) (step ex)
 where
  step (X v                    ) = Map.lookup v env
  step (Lam x               e  ) = Lam x <$> step e
  step (App (Lam x lamBody) arg) = Just (substitute x arg lamBody)
  step (App e1              e2 ) = case step e1 of
    Nothing  -> App e1 <$> step e2
    Just e1' -> Just $ App e1' e2
