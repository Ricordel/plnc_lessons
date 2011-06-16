module StateRPN where

import System.IO
import Data.Char
import Control.Monad

import StateMonad



----------------------------------------
-- Les types nécessaires à l'exercice --
----------------------------------------

type Stack a = [a]
type Operator a = State (Stack a) () -- Un opérateur a seulement un effet de bord sur la pile, pas de résultat



----------------------------------------
------- Parsage des expressions --------
----------------------------------------


-- Factorisation de l'appel à un opérateur binaire

binOp :: Num a => (a -> a -> a) -> Operator a
binOp op = do
                (a : b : rest) <- get
                put $ (op b a) : rest



-- Parsage des opérations autorisées
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


eval :: Num a => [Operator a] -> a
eval stack = let rslt = foldl (>>) init_val stack -- "applattisement" des effets de bord
                    where init_val = State $ \s -> ((), s) -- "identité" de >>
             in
                let (_, x:xs) = runState rslt [] -- récupérer le sommet de la pile
                in
                    x


-- Parsage d'une entrée --
parse :: (Num a, Integral a, Enum a, Read a) => String -> [Operator a]
parse str = map parseOp (words str)



-- Dans cette version, eval applique les différents opérateurs puis retourne le
-- sommet de la pile (et non la State Monad). Les commandes devront donc être entrées en une seule fois
-- car l'état ne sera pas transmis au "prochain tour de forever"
repl :: IO ()
repl = forever $ do
                putStr "> "
                hFlush stdout
                cmd <- getLine
                let rslt = eval $ parse cmd
                putStrLn $ show rslt


main = repl
