module StateMonad where


------------------- The state monad -----------------------

-- Le but est de pouvoir stocker un état, et pouvoir le modifier,
-- en plus d'effectuer les actions monadiques habituelles

-- en effectuant : 
-- mon >>= fun
-- le calcul correspondant à fun est effectué, et l'état est passé
-- (éventuellement modifié) à "la suite"


-- But : pouvoir faire des choses du style :
--drop = State (Stack a) ()
--drop = do
--            (x:xs) <- get
--            put xs


-- State, nouveau type, et instance de Monad
data State s a = State { runState :: s -> (a, s) }


instance Monad (State s) where
    return x = State (\s -> (x, s))

    x >>= f = State (\s ->
                        let (v, s') = runState x s -- exécution de la lambda contenue dans x à partir de l'état s
                        in
                            runState (f v) s' -- on extrait la lambda du résultat de (f v) et on l'applique à l'état
                                              -- actuel, à savoir celui après passage par x
                    )



-- Changer l'état de la monade sans toucher à la valeur (opération duale de return)
put :: s -> State s ()
put state = State (\_ -> ((), state))
-- Action : () = "ne rien faire"
-- Modif de l'état : "se mettre dans l'état 'state'"


-- Récupérer l'état de la monade
get :: State s s
get = State (\s -> (s, s))


-- do
--      x <- get
--      put (f x)
--      return foobar
--
-- est équivalent à :
-- get >>= \x -> do
--              put (f x)
--              return foobar
--
-- i.e :
-- get >>= \x -> (put (f x) >> return foobar)
--
-- Ce qui a pour effet de :
--      "exécuter la monade get", ce qui "recopie" l'état dans la valeur (ce qui est nécessaire car >>= passe la
--      VALEUR à la fonction bindée), et sur la monade qui en résulte, faire :
--          - mettre (f x) comme état
--          - mettre foobar comme valeur
-- 
-- Au final on a donc bien, comme on le désirait, une State monad dans l'état (f x) et avec la valeur foobar, 
-- i.e State (\s -> (foobar, f x))


modify :: (s -> s) -> State s ()
modify f = do
                x <- get
                put $ f x

-- qui est équivalent, sans le sucre sytaxique, à : modify = get >>= put.f


-- Une factorielle qui compte le nombre d'opérations
fact :: Num a => a -> State Int a
fact 0 = State $ \_ -> (1, 0)
fact n = do
            x <- fact(n-1)
            s <- get
            put(s+1)
            return(n*x)


-- Pour tester :
main = runState (fact 10) 0 -- le 0 est indifférent, 42 donnerait le même résultat
