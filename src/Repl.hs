module Repl where

import Parser
import Lambda
import System.IO

repl :: IO ()
repl = do  
  putStr "Lambda: "
  hFlush stdout
  term <- getLine  
  putStrLn $ show (eval $ doParse term)
  repl

strEval :: String -> Term
strEval s = eval $ doParse s