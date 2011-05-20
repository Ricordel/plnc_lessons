USING: tools.test engrammes engrammes.private ;


! === Tests pour prime-factors ===
{ 0 }           [ 0 prime-factors ] unit-test
{ 1 }           [ 1 prime-factors ] unit-test
{ V{ 1 } }      [ 2 prime-factors ] unit-test
{ V{ 0 1 } }    [ 3 prime-factors ] unit-test
{ V{ V{ 1 } } }      [ 4 prime-factors ] unit-test
{ V{ 0 0 1 } }  [ 5 prime-factors ] unit-test
{ V{ 1 1 } }    [ 6 prime-factors ] unit-test
{ V{ V{ 1 } V{ 1 } } }    [ 36 prime-factors ] unit-test
{ V{ V{ 1 1 } } }      [ 64 prime-factors ] unit-test



! === Test de l'encodage d'engrammes, avec les exemples de Natacha Kerensikova ===
{ "0" }         [ 0 >engramme ] unit-test
{ "1" }         [ 1 >engramme ] unit-test
{ "(1)" }       [ 2 >engramme ] unit-test
{ "(01)" }      [ 3 >engramme ] unit-test
{ "((1))" }     [ 4 >engramme ] unit-test
{ "(001)" }     [ 5 >engramme ] unit-test
{ "(11)" }      [ 6 >engramme ] unit-test
{ "(0001)" }    [ 7 >engramme ] unit-test
{ "((01))" }    [ 8 >engramme ] unit-test
{ "(0(1))" }    [ 9 >engramme ] unit-test
{ "(101)" }     [ 10 >engramme ] unit-test
{ "((1)(1))" }  [ 36 >engramme ] unit-test
{ "((11))" }    [ 64 >engramme ] unit-test
{ "((1)(1)101)" } [ 1980 >engramme ] unit-test
{ "(0010000000000000000000000000000000000000000000000000000000000000000000000000001)" } [ 2005 >engramme ] unit-test


! === Test du décodage d'engrammes ===
! Si l'encodage marche (tests ci-dessus), vérifier que les nombres sont bien
! stables par encodage-décodage suffit
{ 0 }    [ 0 >engramme engramme>number ] unit-test
{ 1 }    [ 1 >engramme engramme>number ] unit-test
{ 2 }    [ 2 >engramme engramme>number ] unit-test
{ 3 }    [ 3 >engramme engramme>number ] unit-test
{ 4 }    [ 4 >engramme engramme>number ] unit-test
{ 5 }    [ 5 >engramme engramme>number ] unit-test
{ 6 }    [ 6 >engramme engramme>number ] unit-test
{ 7 }    [ 7 >engramme engramme>number ] unit-test
{ 8 }    [ 8 >engramme engramme>number ] unit-test
{ 9 }    [ 9 >engramme engramme>number ] unit-test
{ 10 }   [ 10 >engramme engramme>number ] unit-test
{ 36 }   [ 36 >engramme engramme>number ] unit-test
{ 64 }   [ 64 >engramme engramme>number ] unit-test
{ 1980 } [ 1980 >engramme engramme>number ] unit-test
{ 2005 } [ 2005 >engramme engramme>number ] unit-test
