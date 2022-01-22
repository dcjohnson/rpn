module Lib
    ( printStack,
      eval
    ) where

import qualified Data.BigDecimal as BD

data StateMachine = StateMachine IsTerminal Transitions
type IsTerminal = Bool
type Transitions = [(String, StateMachine)]

numMachineStart = StateMachine False [("0123456789", numMachinePreDecimal), (".", numMachinePostDecimal)]
numMachinePreDecimal = StateMachine True [("0123456789", numMachinePreDecimal), (".", numMachinePostDecimal)]
numMachinePostDecimal = StateMachine True [("0123456789", numMachinePostDecimal)]

evalMachine :: String -> StateMachine -> Bool
evalMachine [] (StateMachine isTerminal _) = isTerminal
evalMachine (c:rest) (StateMachine isTerminal transitions) =
  let findNewState = (\lC ((lTStr, lSM):rest) -> case (stringHasChar lTStr lC) of
                         True -> Just lSM
                         False -> case rest of
                           [] -> Nothing
                           rest -> findNewState lC rest)
      newState = findNewState c transitions
  in case newState of
    (Just state) -> evalMachine rest state
    Nothing -> False

stringHasChar :: String -> Char -> Bool
stringHasChar s c = foldr (\subC b -> b || (c == subC)) False s

toNum :: String -> Maybe BD.BigDecimal
toNum s =
  case (evalMachine s numMachineStart) of
    True -> return (BD.fromString s)
    False -> Nothing

eval :: [BD.BigDecimal] -> String -> [BD.BigDecimal]
eval (first:second:rest) "+" = [(second + first)] ++ rest 
eval (first:second:rest) "-" = [(second - first)] ++ rest
eval (first:second:rest) "*" = [(second * first)] ++ rest 
eval (first:second:rest) "/" = [(second / first)] ++ rest
eval (first:rest) "dup" = [first, first] ++ rest
eval stack input =
  case (toNum input) of
    (Just num) -> [num] ++ stack
    Nothing -> stack 

printStack :: [BD.BigDecimal] -> IO ()
printStack stack = printStackIndexed 1 stack

printStackIndexed :: Int -> [BD.BigDecimal] -> IO ()
printStackIndexed index (first:rest) = do
  printStackIndexed (index + 1) rest
  putStrLn ((show index) ++ ") " ++ (BD.toString first))
printStackIndexed _ [] = return () 
