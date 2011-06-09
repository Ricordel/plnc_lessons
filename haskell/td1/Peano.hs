module Peano where

import Data.Ratio


data Peano = Zero
            | Succ Peano


-- Le type Peano appartient à de nombreuses type classes, afin d'avoir
-- de pouvoir faire à peu près tout ce qu'on peut imaginer faire avec un type
-- numérique comme celui-là.

instance Num Peano where
    Zero + x = x
    x + Zero = x
    x + Succ y = Succ (x + y)

    x - Zero = x
    Zero - _ = Zero
    (Succ x) - (Succ y) = x - y

    _ * Zero = Zero
    Zero * _ = Zero
    x * Succ y = x * y + x

    signum Zero = 0
    signum _ = 1

    abs = id

    fromInteger 0 = Zero
    fromInteger x = Succ $ fromInteger (x - 1)


instance Integral Peano where
    toInteger Zero = 0
    toInteger (Succ x) = 1 + toInteger x

    quotRem a b = (fromInteger q, fromInteger r)
                where q = div (toInteger a) (toInteger b)
                      r = rem (toInteger a) (toInteger b)

    div a b = fromInteger $ div p q
                where p = toInteger a
                      q = toInteger b



instance Real Peano where
    toRational a = (toInteger a) % 1





instance Eq Peano where
    Zero == Zero = True
    Zero == _ = False
    _ == Zero = False
    Succ x == Succ y = x == y


instance Ord Peano where
    Zero <= _ = True
    _ <= Zero = False
    Succ x <= Succ y = x <= y




instance Enum Peano where
    succ = Succ
    pred Zero = error "No predecessor to Zero"
    pred (Succ x) = x

    fromEnum Zero = 0
    fromEnum (Succ x) = 1 + (fromEnum x)

    toEnum 0 = Zero
    toEnum x = Succ (toEnum $ x-1)

    enumFrom x = map toEnum [(fromEnum x)..]
    enumFromThen x y = map toEnum [(fromEnum x), (fromEnum y)..]
    enumFromTo x y = map toEnum [(fromEnum x)..(fromEnum y)]
    enumFromThenTo x y z = map toEnum [(fromEnum x), (fromEnum y)..(fromEnum z)]




instance Show Peano where
    show Zero = "Z"
    show (Succ a) = "S(" ++ show a ++ ")"

    

instance Read Peano where
    readsPrec _ "Z" = [(Zero, "")]

    -- Forcer la parenthèse fermante ?!
    readsPrec p ('S':'(' : str) = [(Succ rest, str_rest)]
                where [(rest, str_rest)] = let (s, ')') = (init str, last str)
                                            in readsPrec p s

    

    readsPrec p ('S' : str) = [(Succ (rest), str_rest)]
                where [(rest, str_rest)] = readsPrec p str
