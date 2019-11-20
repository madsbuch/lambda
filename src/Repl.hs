module Repl where

import System.Console.Repline
import Control.Monad.State.Strict
import Data.List (isPrefixOf)
import System.Process (system)

import Parser
import Lambda
import System.IO

type Repl a = HaskelineT IO a

cmd :: String -> Repl ()
cmd input = liftIO $ putStrLn $ strEval input

completer :: Monad m => WordCompleter m
completer n = return []

options :: [(String, [String] -> Repl ())]
options = []

ini :: Repl ()
ini = liftIO $ putStrLn "Lambda Calculus Interpreter!"

repl :: IO ()
repl = evalRepl (pure "λ: ") cmd options Nothing (Word completer) ini

strEval :: String -> String
strEval s = case doParseTerm s of
  Right a -> show $ eval a
  Left e -> show e