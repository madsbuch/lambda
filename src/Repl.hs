module Repl where

import System.Console.Repline hiding (options)
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

help :: [String] -> Repl ()
help args = liftIO $ putStr $ "" 
  ++ ":help                Show this help\n"
  ++ ":churchDecode term   Assumning term is church encoded, return the integer\n"
  ++ "                     representation\n"
  ++ ":churchEncode n      encode n as a church numeral\n"

churchEncode :: [String] -> Repl ()
churchEncode [n] = do
  let n' = read n :: Int
  liftIO $ print n'
churchEncode l = help l

genChurch :: Int -> Term
genChurch 0 = strEvalTerm "\\f.\\x.x"


churchDecode :: [String] -> Repl ()
churchDecode [term] = do
  liftIO $ print "Implement me"
churchDecode l = help l

options :: [(String, [String] -> Repl ())]
options = [ ("help", help)
          , ("churchEncode", churchEncode)
          , ("churchDecode", churchDecode)]

ini :: Repl ()
ini = liftIO $ putStrLn "Lambda Calculus Interpreter!"

repl :: IO ()
repl = evalRepl (pure "λ: ") cmd options (Just ':') (Word completer) ini

strEvalTerm :: String -> Term
strEvalTerm s = case doParseTerm s of
  Right a -> eval a
  Left e -> error "err"

strEval :: String -> String
strEval s = case doParseTerm s of
  Right a -> show $ eval a
  Left e -> show e