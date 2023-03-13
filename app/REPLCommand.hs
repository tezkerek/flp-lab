module REPLCommand where

import           Text.Parsec.Language           ( LanguageDef
                                                , emptyDef
                                                )
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Token

data REPLCommand
  = Quit
  | Load String
  | Eval String
  deriving (Show)

replDef :: LanguageDef st
replDef = emptyDef { reservedNames   = [":load", ":quit"]
                   , reservedOpNames = [":l", ":q"]
                   }

replParser :: TokenParser st
replParser = makeTokenParser replDef

replQuit :: Parser REPLCommand
replQuit =
  pure Quit <$> (reservedOp replParser ":q" <|> reserved replParser ":quit")

replLoad :: Parser REPLCommand
replLoad = do
  (reservedOp replParser ":l" <|> reserved replParser ":load")
  s <- many anyChar
  return $ Load s

replEval :: Parser REPLCommand
replEval = Eval <$> many anyChar

replCommand :: Parser REPLCommand
replCommand = replQuit <|> replLoad <|> replEval
