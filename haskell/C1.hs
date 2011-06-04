-- Nom du module courant

module C1 where

-- Au lieu de laisser importer le Prelude implicitement (il contient
-- tout ce qui est prédéfini), on l'importe explicitement pour pouvoir
-- donner notre propre définition de ($) plus bas.

import Prelude hiding (($))

-- Définition de fonctions avec typage explicite et clauses

f :: Integer -> Integer
f 0 = 10
f x | x <= 10 = x * 3
    | otherwise = 100

-- Définition récursive avec typage automatique

fact 0 = 1
fact n = n * fact(n-1)

-- Liste infinie de factorielle

facts = 1 : zipWith (*) [1..] facts

-- Suite (infinie) de Fibonacci

fibos = 1 : 1 : zipWith (+) fibos (tail fibos)

-- Définition d'un nouvel opérateur (///) de manière infix

a /// 0 = 0
a /// b = a `div` b

-- Utilisation de where

g x y | x < y = a + b
      | otherwise = a - b
  where a = x + 1
        b = y * 3

-- Utilisation de let/in

h x y = let a = x + 1
            b = y * 3
          in
                a + b

-- Définition de ($) permettant de jouer sur la précédence de ($)

f $ x = f x

-- Définition de c, fonction constante, avec deux écritures
-- possibles, dont une lambda expression (préfixée par \ qui
-- ressemble visuellement au lambda)

c :: a -> b -> a
c x _ = x
-- c x = \_ -> x

-- Trois écritures de "triple" pour illustrer la currification
-- et les possibilités d'application partielle

triple :: Num a => a -> a -> a -> a
triple a b c = a + b + c
-- triple a = \ b c -> a + b + c
-- triple a = \ b -> (\c -> a + b + c)

-- Identique à "flip" qui inverse l'ordre des deux arguments
-- d'une fonction. Du coup, cela permet l'application partielle
-- du second argument.
partialsecond :: (a -> b -> c) -> b -> a -> c
partialsecond f y x = f x y

-- Cela nous permet de définir cette fonction qui prend deux
-- arguments et ignore le premier avant de retourner le second. 
ss = flip c

-- Définition des opérateurs s, k et i du SK(I) lambda-calcul
-- http://en.wikipedia.org/wiki/SKI_combinator_calculus

k x _ = x
s a b c = a c (b c)
i = s k k

-- Définition de "Maybe" et "Monad". En fait, on la laisse
-- commentée car cela existe déjà dans le Prelude. On déclare
-- que, et comment, Maybe et [] sont des instances de Monad.

-- data Maybe a = Just a | Nothing
--      deriving Show
--
-- fromJust (Just x) = x
-- fromJust Nothing = error "cannot get nothing"
--
-- bind :: (a -> Maybe b) -> Maybe a -> Maybe b
-- bind _ Nothing = Nothing
-- bind f (Just x) = f x
--
-- class Monad m where
--       return :: a -> m a
--
--       fail :: String -> m a
--       fail msg = error msg
--
--       (>>=) :: m a -> (a -> m b) -> m b
--
-- instance Monad Maybe where
--          return = Just
--          fail _ = Nothing
--          Nothing >>= _ = Nothing
--          (Just x) >>= f = f x
--
-- instance Monad [] where
--          return x = [x]
--          fail _ = []
--          l >>= f = concat $ map f l

-- La fonction "minc" incrémente une valeur monadique

minc :: (Monad m, Num a) => a -> m a
minc = return.(+1)

-- Utilisation de "do". Utilisé avec un Maybe, il
-- retournera deux fois sa valeur si elle existe,
-- avec une liste, les éléments de la liste seront
-- ajoutés avec toutes les combinaisons possibles
-- (somme cartésienne). En dessous, l'écriture
-- équivalente n'utilisant que bind (>>=)

xyzzy :: (Monad m, Num a) => m a -> m a
xyzzy x = do
                t <- x
                u <- x
                return (t+u)

xyzzy' x = x >>= (\t ->
             x >>= (\u ->
               return (t+u)))

-- Utilisation du monad IO

myfunc :: Show a => a -> IO()
myfunc x = putStrLn(show x)

myfunc2 :: IO()
myfunc2 = do
                line <- getLine
                putStr "Tapé: "
                putStrLn line

-- Lift permet d'appliquer une fonction non monadique
-- à un monad.

lift :: Monad m => (a -> b) -> m a -> m b
lift f x = x >>= return.f

lift' f x = do
                i <- x
                let j = i
                return(f i)

-- Le programme principal, "main", a comme prototype
-- IO().

main :: IO ()
main = myfunc "foobar"

