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
  l <- getLine
  let nStack = stack ++ [(BD.fromString l)]
    in do
    Lib.printStack nStack
    repl nStack 
