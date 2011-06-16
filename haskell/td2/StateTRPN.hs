module StateTRPN where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import System.IO
import Data.Char


-- Notre type principal sera maintenant
--          StateT (Stack a) IO ()
-- i.e on fera des IO en gardant en mémoire l'état du programme
-- (à savoir la pile)


-- Un opérateur est donc de ce type
type Stack a = [a]
type Operator a = StateT (Stack a) IO ()



-- Factorisation des opérateurs binaires
-- Effet de bord sur la pile, pas d'IO
binOp :: Num a => (a -> a -> a) -> Operator a
binOp op = do
                (a : b : rest) <- get
                put $ (op b a) : rest


-- Les différentes instructions
-- Effets de bord sur la pile, pas d'IO non plus
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

                num -> do
                            st <- get
                            put $ (read num) : st




-- eval consiste à "écraser" les différents opérateurs en un seul
eval :: Num a => [Operator a] -> Operator a -- Operator a = StateT (Stack a) IO ()
eval [] = do
            stack <- get
            lift $ putStrLn (show $ reverse stack) -- effet de bord *dans la monade IO* (lift)
            StateT $ \s -> return ((), s) -- Le StateT "qui fait rien"

eval (op : list) = op >> (eval list)



-- Parse une chaine entrée par l'utilisateur
parse :: (Num a, Integral a, Enum a, Read a) => String -> [Operator a]
parse str = map parseOp (words str)



repl :: (Integral a, Read a, Show a) => StateT (Stack a) IO b
repl = forever $ do
    lift $ putStr "> " -- lift pour faire "rentrer" à l'intérieur du State la fonction qui s'applique à une monade IO
    lift $ hFlush stdout
    line <- lift getLine
    eval $ parse line



main = runStateT repl []
