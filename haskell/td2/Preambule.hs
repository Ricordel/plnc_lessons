module Preambule where

import Data.Maybe
import Control.Monad hiding (forever)


-- Un arbre d'exemple
myTree = Branch (Branch (Leaf 3) (Leaf 2)) (Branch (Leaf 1) (Branch (Leaf 3) (Leaf 5)))


-- Type récursif pour l'arbre
data Tree a = Leaf a | Branch (Tree a) (Tree a)


-- Dans le cas où on choisi MonadPlus == [], on aura la liste des profondeurs de toutes les occurences
-- de l'élément dans l'arbre. Dans le cas de Maybe, on a la première occurence rencontrée
findDepth :: (Eq a, MonadPlus m) => a -> Tree a -> m Int

findDepth e (Leaf x) | x == e = return 0
                     | otherwise = mzero


findDepth e (Branch ag ad) = 
    let pg = findDepth e ag
        pd = findDepth e ad
    in
        (pg `mplus` pd) >>= return.(+1)




-------- Structures de contrôle --------
forever :: Monad m => m a -> m b
forever f =
    do
        f -- agir sur la monade (effet de bord)
        forever f -- et recommencer

-- Equivalent à :
-- forever f = f >> forever f


-- Pour afficher "Hello !" en boucle :
loop_hello :: IO ()
loop_hello = forever $ putStrLn "Hello !"



-- Boucler n fois sur une action monadique
times :: Monad m => Int -> (Int -> m a) -> m ()
times i f | i == 0 = return ()
          | otherwise = do
                            f i
                            times (i-1) f

-- Dire
-- Hello 5
-- ...
-- Hello 1
five_times_hello = times 5 ( \i -> putStrLn $ "Hello " ++ (show i) )
