module REPLCommand where

import           Control.Applicative            ( many )
import           Text.Parsec                    ( anyChar )
import           Text.Parsec.Language           ( LanguageDef
                                                , emptyDef
                                                )
import           Text.Parsec.String             ( Parser )
import qualified Text.Parsec.Token             as Token

data REPLCommand
  = Quit
  | Load String
  | Eval String

replCommand :: Parser REPLCommand
replCommand = undefined
