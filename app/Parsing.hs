module Parsing where

import           Control.Applicative            ( some )
import           Exp
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
                                                ( LanguageDef
                                                , haskellStyle
                                                )
import           Text.ParserCombinators.Parsec.Token

miniHaskellDef :: LanguageDef st
miniHaskellDef = haskellStyle { reservedNames   = ["let", "letrec", "in"]
                              , reservedOpNames = ["\\", "->", ":=", ":", ";"]
                              }

miniHs :: TokenParser st
miniHs = makeTokenParser miniHaskellDef

testParse :: Parser a -> String -> a
testParse p s = case parse p "<input>" s of
  Left  err -> error (show err)
  Right a   -> a

var :: Parser Var
var = Var <$> (identifier miniHs <|> operator miniHs)
-- >>> testParse var "b is a var"
-- Var {getVar = "b"}

varExp :: Parser ComplexExp
varExp = CX <$> var
-- >>> testParse varExp "b is a var"
-- CX (Var {getVar = "b"})

lambdaExp :: Parser ComplexExp
lambdaExp = do
  reservedOp miniHs "\\"
  v <- var
  reservedOp miniHs "->"
  e <- expr
  return $ CLam v e
-- >>> testParse lambdaExp "\\x -> x"
-- CLam (Var {getVar = "x"}) (CX (Var {getVar = "x"}))

letExp :: Parser ComplexExp
letExp = do
  reserved miniHs "let"
  v <- var
  reservedOp miniHs ":="
  e1 <- expr
  reserved miniHs "in"
  e2 <- expr
  return $ Let v e1 e2
-- >>> testParse letExp "let x := y in z"
-- Let (Var {getVar = "x"}) (CX (Var {getVar = "y"})) (CX (Var {getVar = "z"}))

letrecExp :: Parser ComplexExp
letrecExp = do
  reserved miniHs "letrec"
  v <- var
  reservedOp miniHs ":="
  e1 <- expr
  reserved miniHs "in"
  e2 <- expr
  return $ Let v e1 e2
-- >>> testParse letrecExp "letrec x := y in z"
-- LetRec (Var {getVar = "x"}) (CX (Var {getVar = "y"})) (CX (Var {getVar = "z"}))

listExp :: Parser ComplexExp
listExp = List <$> brackets miniHs (commaSep miniHs expr)
-- >>> ghci> testParse listExp "[a,b,c]"
-- List [CX (Var {getVar = "a"}),CX (Var {getVar = "b"}),CX (Var {getVar = "c"})]

natExp :: Parser ComplexExp
natExp = Nat <$> fromInteger <$> natural miniHs
-- >>> ghci> testParse natExp "223 a"
-- Nat 223

parenExp :: Parser ComplexExp
parenExp = parens miniHs expr
-- >>> ghci> testParse parenExp "(a)"
-- CX (Var {getVar = "a"})

basicExp :: Parser ComplexExp
basicExp =
  varExp
    <|> lambdaExp
    <|> letExp
    <|> letrecExp
    <|> listExp
    <|> natExp
    <|> parenExp
-- >>> testParse basicExp "[a,b,c]"
-- List [CX (Var {getVar = "a"}),CX (Var {getVar = "b"}),CX (Var {getVar = "c"})]

expr :: Parser ComplexExp
expr = do
  e <- some basicExp
  return $ foldl1 CApp e
-- >>> testParse expr "\\x -> [x,y,z]"
-- CLam (Var {getVar = "x"}) (List [CX (Var {getVar = "x"}),CX (Var {getVar = "y"}),CX (Var {getVar = "z"})])

exprParser :: Parser ComplexExp
exprParser = whiteSpace miniHs *> expr <* eof
-- >>> ghci> testParse exprParser "let x := 28 in \\y -> + x y"
-- Let (Var {getVar = "x"}) (Nat 28) (CLam (Var {getVar = "y"}) (CApp (CApp (CX (Var {getVar = "+"})) (CX (Var {getVar = "x"}))) (CX (Var {getVar = "y"}))))
