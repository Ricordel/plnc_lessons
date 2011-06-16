module StateTRPN where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import System.IO


-- On réécrit l'interprêteur RPN pour travailler dans une monade de type
--          StateT (Stack a) IO ()
-- i.e on fera des IO en gardant en mémoire l'état du programme (la pile)


-- On a besoin des types suivants
type Stack a = [a]
type Operator a = StateT (Stack a) IO () -- Un opérateur modifie l'état et fait des IO



-- Factorisation des opérateurs binaires
-- Effet de bord sur la pile, pas d'IO
binOp :: Num a => (a -> a -> a) -> Operator a
binOp op = do
                (a : b : rest) <- get
                put $ (op b a) : rest



-- Les différentes instructions
parseOp :: (Num a, Enum a, Integral a, Read a) => String -> Operator a
parseOp s = case s of 
                "+" -> binOp (+)
                "-" -> binOp (-)
                "*" -> binOp (*)
                "/" -> binOp div
                "dup" -> do 
                            (a : rest) <- get
                            put (a : a : rest)

                "swap" -> do
                            (a : b : rest) <- get
                            put (b : a : rest)

                "drop" -> modify tail

                "depth" -> do
                            st <- get
                            put $ toEnum(length st) : st

                "pick" -> do
                            (a : rest) <- get
                            put $ (rest !! (fromEnum a)) : rest

                "clear" -> do
                            put []

                "." -> do
                            (x:xs) <- get
                            lift $ putStrLn (show x) -- lift pour rentrer dans la monate StateT et agit sur la monade IO

                ".s" -> do
                            stack <- get
                            lift $ putStrLn (show $ reverse stack)

                num -> do
                            st <- get
                            put $ (read num) : st




-- eval consiste à "écraser" une liste d'opérateurs en un seul opérateur
eval :: Num a => [Operator a] -> Operator a
eval list = let neutral = StateT $ \s -> return ((), s)
            in
                foldl (>>) neutral list


-- Parse une chaine entrée par l'utilisateur
parse :: (Num a, Integral a, Enum a, Read a) => String -> [Operator a]
parse str = map parseOp (words str)



repl :: (Integral a, Read a, Show a) => StateT (Stack a) IO b
repl = forever $ do
    lift $ putStr "> "
    lift $ hFlush stdout
    line <- lift getLine
    eval $ parse line



main = runStateT repl []
