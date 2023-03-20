module Main where

import           System.Console.Isocline
import           System.IO

import           Eval
import           Exp
import           Parsing
import           Printing
import           REPLCommand
import           Sugar

import           Text.Parsec

main :: IO ()
main = do
  input <- readline "minihaskell"
  case parse replCommand "<input>" input of
    Left  err -> print err >> main
    Right cmd -> case cmd of
      Quit   -> return ()
      Load _ -> putStrLn "Not implemented" >> main
      Eval s -> case parse exprParser "<input>" s of
        Left  err -> print err >> main
        Right e   -> putStrLn (showExp e) >> main
