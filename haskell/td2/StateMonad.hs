module StateMonad where


------------------- The state monad -----------------------

-- Le but est de pouvoir stocker un état, et pouvoir le modifier,
-- en plus d'effectuer les actions monadiques habituelles

-- en effectuant : 
-- mon >>= fun
-- le calcul correspondant à fun est effectué, et l'état est passé
-- (éventuellement modifié) à "la suite"


-- But : pouvoir faire :
{-drop = State (Stack a) ()-}
{-drop = do-}
            {-(x:xs) <- get-}
            {-put xs-}


-- State, nouveau type, et instance de Monad
data State s a = State { runState :: s -> (a, s) }


instance Monad (State s) where
    return x = State (\s -> (x, s))

    x >>= f = State (\s ->
                        let (v', s') = runState x s -- passage par x à partir de l'état s
                            (v'', s'') = runState (f v') s' -- passage par f de la valeur extraite à partir de l'état résultant du passage par x
                        in
                            (v'', s'')
                    )


-- Changer l'état de la monade, opération duale de return
put :: s -> State s ()
put state = State (\_ -> ((), state))
-- Action : () = "ne rien faire"
-- Modif de l'état : "se mettre dans l'état 'state'"


-- Récupérer l'état de la monade
get :: State s s
get = State (\s -> (s, s))


-- Quand on fait
-- do 
--      x <- get
-- qu'est-ce que ça fait exactement ?
--
-- do
--      x <- get
--      put (x+1)
--      return 3
--
-- se traduit en
-- get >>= f
--  where
--      f x = do put(x+1)
--               return 3
--      f _ = fail "truc"
--
--
-- qui à son tour se traduit en :
--
-- get >>= f where 
--  f x = put(x+1) >> return 3
--  f _ = fail "..."
--
--
--  soit encore
--  let f x = put(x+1) >> return 3
--  in
--      get >>= f -- f est bien du bon type
--
--  Or ce get >>= f fait :
--      "exécuter la monade get"
--      exécuter f, qui à son tour consiste à :
--          - changer l'état pour le mettre à la valeur de son argument + 1 (car en fait f = put.(+1)), argument qui vaut la valeur
--            retournée par l'exécution de get, à savoir l'état de la monade (dans l'implémentation de >>=
--            on a ben **exécuté** la lambda contenue dans la monade, dont on a bien un couple en retour)
--          - ne rien faire sur la valeur (car l'effet de bord de put est () )
--
-- On a donc bien l'effet voulu


modify :: (s -> s) -> State s ()
modify f = do
                x <- get
                put $ f x

-- qui est équivalent, en notation monadique, à : modify = get >>= put.f


fact :: Num a => a -> State Int a
fact 0 = State $ \_ -> (1, 0)
fact n = do
            x <- fact(n-1)
            s <- get
            put(s+1)
            return(n*x)
