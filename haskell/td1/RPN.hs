module RPN where

import System.IO
import Data.Char


----------------------------------------
-- Les types nécessaires à l'exercice --
----------------------------------------

-- Ensuite, on paramètrera
type Stack = [Int]
type Operator = Stack -> Stack



----------------------------------------
------- Parsage des expressions --------
----------------------------------------


-- Factorisation de l'appel à l'opérateur binaire
binOp :: (Int -> Int -> Int) -> Operator
binOp op (a : b : rest) = (op b a) : rest -- ordre inversé pour - et /



-- Parsage des opérations autorisées
parseOp :: String -> Operator
parseOp s | s == "+" = binOp (+)
          | s == "-" = binOp (-)
          | s == "*" = binOp (*)
          | s == "/" = binOp div
          | s == "dup" = \(a : rest) -> a : a : rest
          | s == "swap" = \(a : b : rest) -> b : a : rest
          | s == "drop" = tail
          | s == "depth" = \st -> (length st) : st
          | s == "pick" = \(a : rest) -> (rest !! a) : rest
          | s == "clear" = \_ -> []

parseOp num  = \st -> (read num) : st



-- Evaluation d'une suite d'opérateurs --
eval :: Stack -> [Operator] -> Stack
eval st [] = st
eval st (a : rest) = eval (a st) rest


-- Parsage d'une entrée --
parse :: String -> [Operator]
parse str = map parseOp (words str)



-- Boucle principale pour tester notre mini-factor --
repl :: Stack -> IO ()
repl stack = do
    putStr "> "
    hFlush stdout
    line <- getLine
    newstack <- return $ eval stack (parse line)
    putStrLn $ show $ reverse newstack
    repl newstack

main = repl []
