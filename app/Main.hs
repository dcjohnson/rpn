module Main where

import qualified Lib as Lib
import System.IO
import qualified Data.BigDecimal as BD

main :: IO ()
main = repl []    

repl :: [BD.BigDecimal] -> IO ()
repl stack = do
  putStr "> "
  hFlush stdout
  input <- getLine
  case input of
    "exit" -> return ()
    input -> let nStack = Lib.eval stack input
      in do
      Lib.printStack nStack
      repl nStack 
