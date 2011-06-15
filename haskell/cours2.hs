module cours2 where

----------------------------------------------------------
--              Les monades II, le retour               --
----------------------------------------------------------

do
    x <- y
    f x

-- est traduit en 
y >>= (\x -> f x)
-- soit encore
y >>= f



------ Quelques lois pour return et bind --------

-- Identité à gauche
return a >>= f == f a -- f retourne une monade !!
-- Identité à drotie
x >>= return == x
-- Associativité
(x >>= f) >>= g == x >>= (\y -> f y >>= g)


-- Composition de Kleisli
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
(f >=> g) x = (f x) >>= g

-- On peut réécrire les lois ci-dessus (de manière équivalente) : 
return >=> f == f
f >=> return == f
(f >=> g) >=> h == f >=> (g >=> h)

-- On peut regarder l'extension sur les flèches si on veut



---------- Retour sur lift : transormer une foncton normale en fonction monadique
-- fait rentrer la fonction dans la monade pour l'appliquer
(Just "Foobar") >>= return.length -- retourne Just 6
-- équivalent à :
lift length $ Just "Foobar"

-- On redéfini lift :
lift ::Monad m => (a -> b) -> m a -> m b
lift f x = x >>= return.f



--------- Construction de types ----------
data Maybe a = Just a | Nothing

-- On voudrait une Personne qui contient des infos sur qqn
data Personne = Personne String String deriving Show
Personne "John" "Doe"

first_name :: Personne -> String
first_name (Personne f _) = f

last_name (Personne _ l) = l

-- On voudrait que les extracteurs soient construits automatiquement -> nommer les champs :
data Personne = Personne (first_name :: String, last_name :: String) deriving Show

-- Pour Maybe :
data Maybe a = Just (fromJust :: a) | Nothing
-- mais sera codé comme :
fromJust (Just a) = a
-- et causera une erreur s'il est utilisé avec Nothing



------------------ La type class MonadPlus ------------------------
-- Ajoute mzero
mzero >>= f = mzero -- absorbant pour le bind
x >> mzero = mzero -- l'opérateur >> s'utilise pour un m b, et pas a -> m b

-- Pour un Maybe, le mzero est Nothing
-- Pour une liste : mzero = []

-- L'opération mplus
[1,2,3] `mplus` [4,5,6] -- donnera [1,2,3,4,5,6]

(Just "foo") `mplus` Nothing -- Just "foo"
(Just "foo") `mplus` (Just "bar") -- Just "foo"
-- à voir comme une évaluation paresseuse de booléens si on veut...

-- msum fait un foldl avec mplus
msum [[1,2], [3,4], [5,6]] = [1,2,3,4,5,6]
msum [Nothing, Nothing, Just "foo", Nothing, Just "bar"] = Just "foo" -- Retourne le premier qui n'est pas Nothing

msum [] = mzero
msum (x:xs) = x `mplus` (msum xs)
-- Marchera donc sur tous les monades plus



-------------- La monade Error ---------------
-- Comme un Nothing augmenté




-------------- Le type Either (pas une monade pour des questions de typage) ----------------
data Either a b = Left a | Right b

-- Il existe une fonction (cf Hoogle) tq :
apply :: (a -> c) -> (b -> c) -> Either a b -> c





------------------ La State Monade ------------------
-- Une monade qui contient un objet (comme une monade normale) + un état
data State s a = State (runState :: s -> (a, s))

-- Comme on ne sait pas stocker deux choses dans une monade (sinon il faudrait agir sur les 2 éléments), 
-- on préfère garder une fonction qui retourne les deux choses
instance Monad (State s) where
    return x = State $ \s -> (x, s)
    -- on veut en fait qu'un appel du genre
    -- runState (return "foobar") 18 ==> ("foobar", 18)

    x >>= f = State $ \s ->
                        let (r, s') = runState x s -- extraction + passage de l'état par x
                            (r', s'') = runState (f r) s' -- Appel de f + passage par f
                        in (r', s'')

-- State x :: initial -> (valeur, final)
-- f :: valeur -> (initial -> (valeur, final))


runState (return 3) 18 -- (3, 18)
runState (return 3 >>= (\x -> State $ \s -> (x+1, s*2))) 18 -- (4, 36)


-- On veut pouvoir faire, dans un do :
--      mystate <- get
--      put (mystate + 5)
--      return 17


get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put newState = State $ \s -> ((), newState)

modify :: (s -> s) -> State s ()
modify f = get >>= put.f



-- Une fonction factorielle qui compte les multiplications
fact :: Int -> State Int Int -- Le premier Int est l'état = nb_opérations, le 2è est le type de la valeur = fact
fact x | x < 3 = return x
       | otherwise = do
                        temp <- fact (x-1) -- fact (x-1) retourne une monade -> on extrait la valeur
                        current_state <- get
                        put (current_state + 1) -- pour ajouter une multiplication
                        return (x * temp)

runState (fact 5) 0 -- retourne (120, 3), 3 car on a commencé à 2


fact :: Int -> State String Int -- Le premier Int est l'état = nb_opérations, le 2è est le type de la valeur = fact
fact x | x < 3 = return x
       | otherwise = do
                        temp <- fact (x-1) -- fact (x-1) retourne une monade -> on extrait la valeur
                        modify ('.':) -- fonction qui ajoute un ':' en tête
                        return (x * temp)

runState (fact 5) "" -- donnera (120, "...")

-- au moment de l'appel à fact 5, rien ne s'est passé, on a juste retourné un énorme empilement de lambdas expliquant
-- comment procéder au moment du passage dans runState




putStrLn :: String -> IO ()
-- On peut aussi l'imaginer comme :
String -----> IO ----> un String -> () mais à l'intérieur de la monade IO

putMaybeStrLn :: Maybe String -> IO ()

getLine :: IO String -- qu'on va appliquer dans un bind

getMaybeLineIfPresent :: IO (Maybe String)

getMaybeLineIfPresent :: MaybeT IO String -- MaybeT IO est une monade
-- MaybeT est un transformeur de monade
getMaybeLineIfPresent = do
                            line <- lift getLine -- on peut quand même travailler à l'intérieur de la monade IO
                            if not $ isEmpty line then return line else fail "Empty line"



--- Translation from do-notation to monadic notation
do 
    rslt <- action
    o_rslt <- o_action
    (action_based_on_results rslt o_rslt)

-- gets translated into

action >>= f
where f rslt = do
                    o_rslt <- o_action
                    (action_based_on_results rslt o_rslt)
      f _ = fail "..."



do
    action
    other_action
    yet_another_action

-- se traduit par

action >> do
            other_action
            yet_another_action


-- et ainsi de suite de manière récursive
