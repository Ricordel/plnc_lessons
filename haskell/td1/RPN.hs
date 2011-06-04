module RPN where

import System.IO
import Data.Char
import Peano


----------------------------------------
-- Les types nécessaires à l'exercice --
----------------------------------------

-- Ensuite, on paramètrera
type Stack a = [a]
type Operator a = Stack a -> Stack a


----------------------------------------
------- Parsage des expressions --------
----------------------------------------


-- Factorisation de l'appel à l'opérateur binaire
binOp :: Num a => (a -> a -> a) -> Operator a
binOp op (a : b : rest) = (op b a) : rest



-- Parsage des opérations autorisées
parseOp :: (Num a, Enum a, Integral a, Read a) => String -> Operator a
parseOp s = case s of 
                "+" -> binOp (+)
                "-" -> binOp (-)
                "*" -> binOp (*)
                "/" -> binOp div
                "dup" -> \(a : rest) -> a : a : rest
                "swap" -> \(a : b : rest) -> b : a : rest
                "drop" -> tail
                "depth" -> \st -> (toEnum $ length st) : st
                "pick" -> \(a : rest) -> (rest !! (fromEnum a)) : rest
                "clear" -> \_ -> []
                num -> \st -> (read num) : st



-- Evaluation d'une suite d'opérateurs --
eval :: Num a => Stack a -> [Operator a] -> Stack a
eval st [] = st
eval st (a : rest) = eval (a st) rest


-- Parsage d'une entrée --
parse :: (Num a, Integral a, Enum a, Read a) => String -> [Operator a]
parse str = map parseOp (words str)



-- Boucle principale pour tester notre mini-factor --
{-repl :: (Num a, Integral a, Enum a, Read a) => Stack a -> IO ()-}
repl :: Stack Peano -> IO ()
repl stack = do
    putStr "> "
    hFlush stdout
    line <- getLine
    newstack <- return $ eval stack (parse line)
    putStrLn $ show $ reverse newstack
    repl newstack

main = repl []
