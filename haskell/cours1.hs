-- Haskell est lazy : les choses ne sont évaluées qu'en cas de besoin.
-- Par ex, on peut très bien écrire x = 4 / 0, pas de problème tant qu'on n'évalue pas.

> Ceci est un commentaire de documentation


--------------------- Quelques mots sur le REPL -------------------------
:t 1 -- donne le type de 1, qui est : 1 :: Num a => a
-- cela veut dire que c'est un type numérique.
-- en Haskell, on utilise des type classes
:t "foobar" -- donne [Char] i.e liste de chars
:t [1.5, 1, 3] -- Fractional t => [t]
:t 'X' -- Char



--------------------- Les opérateurs ------------------------
-- Les opérateurs sont des fonctions qui ont une notation infixe :
1 + 2

-- infixe => préfixe mettre des ()
(+) 1 2

div 5 2 -- division entière

-- préfixe => infixe : mettre des ` `
5 `div` 2

-- Application partielle :
(+ 5) -- est de type a0 -> a0
-- Le repl ne sait pas l'afficher car les fonctions ne sont pas dans la type class show

(5 +) -- Num a => a -> a

-- Les fonctions prennent un seul argument, mais peuvent retourner d'autres fonctions, forme curryfiée



-- La fonction read :
(read "True") :: Bool

-- Différence entre Int et Integer : Integer est plus grand



----------------------------- Les fonctions ------------------------------

-- Pur charger ce fichier dans le repl : :load <nom_du_module>
-- Haskell fait du pattern matching sur les déclarations de fontions, dans l'ordre :

f :: (Num a, Ord a) => a -> a
-- On ne peut pas être moins précis que ce qui sera inféré, mais on peut restreindre pour être
-- plus précis, on pourrait mettre :
f :: Integer -> Integer
-- mais ça n'aurait pas d'intérêt
f 0 = x
f x | x < 10 = x * 3
	| otherwise = 100


a = div 1 0 -- pas de problème
l = [a, a, a] -- pas de problème
-- Il n'y aura pas de problème tant qu'on n'a pas à évaluer a


----------- La factorielle -------------

-- La fact "classique" mais pas idiomatique
fact 0 = 1
fact n = n * fact(n-1)

-- La façon liste paresseuse :
-- Le cons se note ":" qui s'associe à droite
-- tête : queue, exactement comme t :: q en Caml

-- zipWith : 
zipWith (*) [1, 2, 3] [4, 5, 6] -- multiplie 2 à 2
-- take (extraction de sous-liste), length, ...
take 5 1.. -- prend les 5 premiers éléments de la liste des entiers à partir de 1
[1,8..100]

facts = 1 : zipWith (*) [1..] facts
take 10 facts -- donnera bien les 10 premières factorielles
-- Ici on a une structure infinie récursive



-- La fonction error :
error "Foobar"
:t error -- error :: [Char] -> a, peut retourner npq car doit marcher partout

f _ = error "foobar" -- le _ = don't care


-- Fibo avec une liste infinie :
fibos = 1 : 1 zipWith (+) fibos (tail fibos)


-- Définissions +++ sur des entiers
(+++) a b = a + b -- comme on l'a défini entre (), +++ sera infixe par défaut
-- mieux :
a +++ b = a + b -- :: Num a => a -> a -> a

-- avec des gardes :
a /// 0 = 0
a /// b = a `div` b

-- Haskell peut utiliser l'indentation pour deviner des choses :

g x y = a * b
	where a = x + 1
	      b = x * 3
-- Si les clauses where ne sont pas alignées, parseError !!!


h x y = let a = x + 1
			b = y * 3
		in
			a + b

-- Fonctions avec arguments multiples : associativité à gauche, prioritaire sur à peu près tout
g 4 (g 3 5) -- Comme en Caml

-- Mais les parenthèses sont moches, on peut utiliser $ qui évalue ses arguments gauche et droit.
-- et est de très faible priorité
g 10 $ g 3 4

-- Il se définirait par :
f $ x = f x
-- et n'est utile que par sa priorité minimale

-- Si on veut vraiment le redéfinir
import Prelude hiding (($))
-- PENSER À METTRE LE $ DANS UNE COULEUR SPÉCIALE !



-- Le combinateur K de plusieurs manières
k :: a -> b -> a
k x y = x

k :: a -> b -> a
k x _ = x

-- Le \ dénote le lambda
k :: a -> b -> a
k x = \y -> x -- la valeur de x est captée lorsque c est appelée
-- Ou :
k x = \_ -> x -- la valeur de x est captée lorsque c est appelée


-- Une application partielle sur le 2è argument
partialsecond :: ( a -> b -> c) -> b -> (a -> c)
partialsecond f y x = f x y
-- On pourrait aussi l'appeler flip (ou C...)
(flip K 3) -- vaut l'identité

-- flip K = C K

sec = flip c -- Sec 10 20 retournera 20

s x y z = x z (y z)

i = s k k
-- Avec le typage d'Haskell, on ne peut pas déclarer Y car il n'est pas typable


-- Le type Maybe, qui correspond aux options
data Maybe a = Just a | Nothing -- il faut masquer Monad(..) et Maybe(..) si on veut redéfinir
-- Ceci est un type somme

-- On ne peut pas afficher Nothing car on ne dérive pas de la typeclass show
data Maybe a = Just a | Nothing
	deriving Show

fromJust (Just x) = x
fromJust Nothing = error "Cannot get Nothing"
-- maintenant, fromJust :: Maybe t -> t

bind :: (a -> Maybe b) -> Maybe a -> Maybe b
bind _ Nothing = Nothing
bind f (Just x) = f x

f = x -> Just(x+1)
bind f Nothing -- vaut bien Nothing
bind f (Just 4) -- vaut bien Just 5


-- La même chose avec une liste :
bind f [4, 5, 6] -- qui retournerait [5, 6, 7]


------- Début des monades ---------
-- On va alors se faire une typeclass = ensemble de types qui ont une propriété en commun
-- par exemple la propriété "accepter une fonction bind"

class Monad m where
	-- Une liste de fonctions à fournir pour être une monade, comme un trait...
	return :: a -> m a -- permet de construire une valeur

	fail :: String -> m a
	fail msg = error msg -- on peut donner une implémentation par défaut

	(>>=) :: m a -> ( a -> m b) -> m b -- lire "bind"
	

-- Maybe est une monade :
instance Monad Maybe where
	return = Just
	fail _ = Nothing
	Nothing >>= _ = Nothing
	Just x >>= f = f x


f x = return (x + 1)
(f 3) :: Maybe Int -- Just 4
(Just 17) >>= f >>= f -- Just 19


instance Monad [] where
	return x = [x]
	fail _ = []
	[] >>= f = []
	l >>= f = concat $ map f l


-- fonction concat : qui prend une List[List] et retourne une List

minc :: (Monad m, Num a) => a -> m a
minc = return.(+1) -- . est l'opérateur de composition

xyzzy :: (Monad m, Num a) => m a -> m a
xyzzy x = do
				t <- x
				u <- x
				{-return minc(t)-}
				return (t+u)
-- on peut voir ça comme le for de Scala

-- do est du sucre syntaxique qui permet d'écrire des séries de bind
-- équivalent à :
{-xyzzy' x = x >>= (\t -> minc(t))-}

xyzzy' x = x >>= (\t -> 
				x >>= (\u ->
					return (t+u)))

xyzzy [3] -- retournera [6]
xyzzy [3, 4] -- donne [6, 7, 7, 8] !! concatène des listes, puis RE-concatène des listes




------------------------------
--			Les I/O			--
------------------------------

-- Quelle la signature de putStr ?
-- String -> IO ()
-- IO est une monade

-- Et getLine ?
-- getLine :: IO String
-- Ce n'est pas une fonction, mais une valeur qui est une monade

-- La monade IO n'a pas de constructeur ni de déconstructeur. Il va donc falloir acquérir une
-- monade IO, puis agir dessus avec des bind

main :: IO ()
main = do
			myFunc("foobar")

-- Une fonction qui affiche un truc
myfunc :: Show a => a -> ()
myfunc x = let a = putStrLn(show x) -- la fonction show de la type class Show est un toString
			in ()
-- ne fera rien ! la variable a n'est pas utile => elle n'est pas évaluée => putStrLn n'est pas appelée


-- Ce qu'il faut faire :
myfunc :: Show a => a -> IO ()
myfunc x = putStrLn(show x)

main = do
			myfunc "foobar"

-- Ou même :
main = myfunc "foobar"


-- Demander une entrée à l'utilisateur
getLine >>= putStrLn -- va bien : le >>= de la monade IO attend un IO qqch et agit dessus
myfunc2 :: IO()
myfunc2 = do
			line <- getLine
			putStr "Tapé : "
			putStrLn line

-- Il y a un autre bind : >>
-- est en fait traduit en :
myfunc2 = getLine >>= (\line ->
						putStr "Tapé : " >> 
							(putStrLn line))


----- Manipuler la valeur présente à l'intérieur d'une monade -----
lift :: Monad m => (a -> b) -> m a -> m b
lift f x = x >>= return.f
-- équivlent à :
lift f x = do
				i <- x
				return(f i)

lift lenth getLine -- attend une chaine sur stdin et affichera la longueur :
-- on a manipulé à l'aide de getLine la valeur qui se trouve à l'intérieur de la
-- monade IO



-------- La doc : Hoogle ------------
-- Ex : entrer un schéma de types, hoogle donnera des fonctions acceptant ce schéma de type
-- puis celles contenant ce schéma de types dans son type


repeat x = x : (repeat x)

-- regarder sequence pour retourner une Monade contenant une liste au lieu d'une liste de monades

-- dans do, on peut utiliser let (sans in)
do
		i <- x
		let j = i
		return (f i)

-- D'où le fait qu'on doive mettre les let partout dans la REPL : la REPL est une monade
-- Attention toutefois : le do prend une monade et retourne le même type de monade. Par exemple
-- dans la REPL, on est dans une monade IO, donc on ne peut pas faire directement i <- [1, 2, 3]
-- ce qu'on voit bien dans les types : m a -> m b


-- Il y a des variantes func_ pour pas mal de fonctions
