module Main where

import qualified Lib as Lib
import System.IO
import qualified Data.BigDecimal as BD

main :: IO ()
main = repl [] ""

repl :: [BD.BigDecimal] -> String -> IO ()
repl stack lastCommand = do
  putStr "> "
  hFlush stdout
  input <- getLine
  case input of
    "exit" -> return ()
    "" -> let nStack = Lib.eval stack lastCommand
      in do
      Lib.printStack nStack
      repl nStack lastCommand
    input -> let nStack = Lib.eval stack input
      in do
      Lib.printStack nStack
      repl nStack input
