module Lib
    ( printStack,
      eval
    ) where

import qualified Data.BigDecimal as BD
import System.IO

data StateMachine = StateMachine IsTerminal Transitions
type IsTerminal = Bool
type Transitions = [(String, StateMachine)]

digits :: String
digits = "0123456789"

dot :: String
dot = "."

numMachineStart = StateMachine False [(digits, numMachinePreDecimal), (dot, numMachinePostDecimal)]
numMachinePreDecimal = StateMachine True [(digits, numMachinePreDecimal), (dot, numMachinePostDecimal)]
numMachinePostDecimal = StateMachine True [(digits, numMachinePostDecimal)]

pureIntMachineStart = StateMachine False [(digits, pureIntMachineEnd)]
pureIntMachineEnd = StateMachine True [(digits, pureIntMachineEnd)]

moveMachine = StateMachine False [("m", StateMachine False [("o", StateMachine False [("v", StateMachine False [("e", StateMachine False [(" ", pureIntMachineStart)])])])])]

deleteMachine = StateMachine False [("d", StateMachine False [("e", StateMachine False [("l", StateMachine False [("e", StateMachine False [("t", StateMachine False [("e", StateMachine False [(" ", pureIntMachineStart)])])])])])])]

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

data Instruction
  = Number
  | Move
  | Delete
  | Nop

data Parser a b = Parser StateMachine Instruction

getType :: String -> Instruction
getType s =
  let getTypeHelper = (\parsers ->
                         case parsers of
                           ((Parser sm i):rest) ->
                             case (evalMachine s sm) of
                               True -> i
                               False -> getTypeHelper rest
                           [] -> Nop)
  in getTypeHelper [Parser numMachineStart Number, Parser moveMachine Move, Parser deleteMachine Delete]

getIntArg :: String -> Int
getIntArg s =
  let args = words s
  in read (args !! 1)

eval :: [BD.BigDecimal] -> String -> [BD.BigDecimal]
eval (first:second:rest) "+" = (second + first):rest 
eval (first:second:rest) "-" = (second - first):rest
eval (first:second:rest) "*" = (second * first):rest 
eval (first:second:rest) "/" = (second / first):rest
eval stack "nroot" =
  case stack of
    (BD.BigDecimal n f):second:rest ->
      case (f == 0) of
        True ->
          let nBD = BD.BigDecimal n f
              newton = (\xK -> (xK - (BD.divide (((xK ^^ n) - second), (xK ^^ (n - 1)) * nBD) (BD.HALF_UP, Just 30))))
              newtonPrecision = (\decimalPrecision x ->
                                   let xK = newton x
                                       xkdiff = abs (x - xK)
                                   in case (xkdiff < decimalPrecision) of 
                                     True -> xK
                                     False -> newtonPrecision decimalPrecision xK)
          in (newtonPrecision (BD.BigDecimal 1 30) (BD.BigDecimal 2 0)):rest
        False -> stack
    _ -> stack
eval stack "^" =
  case stack of
    (BD.BigDecimal n f):second:rest ->
      case (f == 0) of
        True -> (second ^^ n):rest
        False -> stack
    _ -> stack
eval (first:rest) "dup" = [first, first] ++ rest
eval (first:rest) "pop" = rest
eval (first:rest) "rot" = rest ++ [first]
eval stack "clear" = []
eval stack input =
  case (getType input) of
    Number -> [(BD.fromString input)] ++ stack
    Move ->
      let moveHelper =
            (\targetIndex targetValue tail head ->
                case tail of
                  (te:rTail) -> 
                    case (targetIndex == 2) of
                      True -> head ++ [te, targetValue] ++ rTail
                      False -> moveHelper (targetIndex - 1) targetValue rTail (head ++ [te])
                  [] -> targetValue:head)
          index = getIntArg input
      in case (index <= 1) of
        True -> stack
        False ->
          case stack of
            (targetValue:rest) -> moveHelper index targetValue rest []
            _ -> stack
    Delete ->
      let deleteHelper =
            (\index (te:tail) head ->
                case (index == 1) of
                  True -> head ++ tail
                  False -> deleteHelper (index - 1) tail (head ++ [te]))
          index = getIntArg input
      in case (index > (length stack) || index == 0) of
        True -> stack
        False -> deleteHelper index stack []
    Nop -> stack 

printStack :: [BD.BigDecimal] -> IO ()
printStack stack = printStackIndexed 1 stack

printStackIndexed :: Int -> [BD.BigDecimal] -> IO ()
printStackIndexed index (first:rest) = do
  printStackIndexed (index + 1) rest
  putStrLn ((show index) ++ ") " ++ (BD.toString first))
printStackIndexed _ [] = return () 
