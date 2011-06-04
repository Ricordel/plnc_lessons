module CheckPeano where

import Peano
import Monad
import Test.QuickCheck

instance Arbitrary Peano where
	arbitrary = liftM toEnum $ choose (0, 10)
	shrink (Succ a) = [a]
	shrink Zero = []

prop_showread p = read (show p) == p
        where types = p :: Peano

prop_showread_noparen p = read (filternot (`elem` "()") $ show p) == p
        where types = p :: Peano

filternot p = filter (\x -> not $ p x)

prop_eq p = p == p
        where types = p :: Peano

prop_neq p = p /= Succ(p)
	where types = p :: Peano

prop_gt p1 p2 = (toInteger p1 > toInteger p2) == (p1 > p2)
	where types = p1 :: Peano

prop_comm_add p1 p2 = p1 + p2 == p2 + p1
        where types = p1 :: Peano

prop_comm_mul p1 p2 = p1 * p2 == p2 * p1
        where types = p1 :: Peano

prop_add_sub p1 p2 = (p1 + p2) - p1 == p2
        where types = p1 :: Peano

prop_mul_div p1 p2 = p1 /= Zero ==> (p1 * p2) `div` p1 == p2
        where types = p1 :: Peano

prop_abs p = p == abs(p)
        where types = p :: Peano

prop_signum p = signum p == if p == Zero then Zero else Succ Zero
        where types = p :: Peano

prop_to_integer p = (toInteger p) + 1 == (toInteger (Succ p))
         where types = p :: Peano

prop_from_integer i = i >= 0 ==> Succ (fromInteger i) == fromInteger (i+1)
         where types = i :: Integer

prop_involution i = i >= 0 ==> toInteger (fromInteger i) == i


main = do
        quickCheck prop_showread
        quickCheck prop_showread_noparen
        quickCheck prop_eq
        quickCheck prop_neq
        quickCheck prop_gt
        quickCheck prop_comm_add
        quickCheck prop_comm_mul
        quickCheck prop_add_sub
        quickCheck prop_mul_div
        quickCheck prop_abs
        quickCheck prop_signum
        quickCheck prop_to_integer
        quickCheck prop_from_integer
