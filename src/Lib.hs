module Lib
    ( printStack
    ) where

import qualified Data.BigDecimal as BD

printStack :: [BD.BigDecimal] -> IO ()
printStack stack = printStackIndexed 1 stack

printStackIndexed :: Int -> [BD.BigDecimal] -> IO ()
printStackIndexed index (first:rest) = do
  printStackIndexed (index + 1) rest
  putStrLn ((show index) ++ ") " ++ (BD.toString first))
printStackIndexed _ [] = return () 
