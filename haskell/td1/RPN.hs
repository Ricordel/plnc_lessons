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
parseOp "+" = binOp (+)
parseOp "-" = binOp (-)
parseOp "*" = binOp (*)
parseOp "/" = binOp div
parseOp "dup" = \(a : rest) -> a : a : rest
parseOp "swap" = \(a : b : rest) -> b : a : rest
parseOp "drop" = tail
parseOp "depth" = \st -> (length st) : st
parseOp "pick" = \(a : rest) -> (rest !! a) : rest
parseOp "clear" = \_ -> []
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
