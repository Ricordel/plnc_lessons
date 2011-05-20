! ===== Les espaces de nommage, les variables =====

! Les expaces de nom sont en fait des assoc

\ set see

"foo" "bar" set ! la variable bar contient "foo"
! Le nom de variable est totalement libre

! Éviter les fautes de frappe : utiliser des symboles :
SYMBOL: foo
17 foo set
foo get
! S'il y a une erreur, elle est maintenant détectée à la compilation
! => toujours utiliser des symboles comme identificateurs de variable

! Les namespaces with-scope :
SYMBOL: name
"Yoann" name set

[ "titi" name set name get . ] with-scope
! aura créé un nouveau namespace, en tête de la liste des namespaces.
name get ! appelé ensuite retourne bien "Yoann"

: print-name ( -- ) name get print
[ "Toto" name set print-name ] with-scope ! marche aussi

"Titi" name [ print-name ] with-variable ! aura fait un "Titi" name set dans un nouveau scope

! with-string-writer se contente de redéfinir output-stream dans un nouveau
! namespace

! !!! Les namespaces ne sont pas statiques comme en C++ ou Java
"Titi" name [ [ name get print ] ] with-variable
! Retourne une quotation, qui, quand elle sera exécutée, utilisera la valeur de name dans le
! namespace courant => "ATTENTION À ÇA !!!"
: change-name ( name -- ) name set
"Sam" change-name ! => retournera bien "Sam" plus loin
[ "Titi" change-name ] with-scope
name get ! retournera "Sam", car le change-name a été effectué dans l'espace de nom le plus globale
! set-global ou get-global pour pusher dans l'espace de nom de niveau supérieur => moche


! === La currification : curry ===
3 [ + ] curry ! retournera [ 3 + ]

: adder ( x -- quot ) [ + ] curry
5 adder ! retournera une quotation qui, une fois appliquée, ajoutera 5 à l'élément sur la pile

: multi-at ( keys assoc -- values )
    [ at ] curry
    map
;

! compose : compose deux quotations :
[ 3 + ] [ 2 * ] compose



! bi-curry
: muldivq ( n -- mulquot divquot )
    [ [ * ] curry ] [ [ / ] curry ] bi
    ! OU
    dup [ [ * ] curry ] [ [ / ] curry ] bi* ! ??

    [ * ] [ / ] bi-curry ! Fait rentrer l'élément sur la pile dans ces deux quotations
    ! bi-curry = [ curry ] bi@ bi

! souvent suivi de bi pour appliquer directement les quotations nouvellement créées



! ===== Les exceptions =====
! throw : prend une erreur, et retourne *
! On peut lever n'importe quel objet
"Toto" throw
ERROR: failure ; ! Equivalent à TUPLE: failure, ET déclare un mot qui créé un nouvel objet failure et le lève
ERROR: bad-parameter n ;


! le boa
TUPLE: personne first last ;
person new ! créé un tuple où les champs valent false
! boa permet de prendre les valeurs sur la pile :
"Samuel" "Tardieu" personne boa ! met Samuel dans first, et Tardieu dans last

ERROR: bad-parameter n ; ! utilise boa pour créer une nouvelle instance de l'exception

! Personnaliser le message d'erreur
M: bad-parameter summary "Bad parameter"
17 bad-parameter

! recover : [ quot ] [ en cas d'erreur ] recover
: db ( a b -- a/b )
    [ / ] [ 3drop 0 ] recover ! 3drop car il faut virer les 2 paramètres ET l'objet d'erreur
;

: db ( a b -- a/b )
    [ / ] [ 2nip ] recover ! On veut laisser l'erreur sur la pile
;

: db ( a b -- a/b )
    [ / ] [ 2nip x>> ] recover ! laisse le numérateur
;

! cleanup : pour un try ... catch ... finally
! MAIS : dans l'ordre : [ try ] [ finally ] [ catch ], et en fait le finally est exécuté AVANT le reste
! S'utilise environ jamais
! Relève l'exception, ne la supprime pas
: db ( a b -- a/b )
    [ / ]
    [ computation terminated ]
    [ "Exception raised" print ]
    cleanup
;



! ===== Noms spéciaux pour les paramètres =====

! Si les param sont des quotations, on peut indiquer les stack effect des quotations.
! Les noms qui commencent par ".." représente une profondeur de pile qcq, mais si on réutilise le
! même nom, ça représentera la même profondeur de pile. Ils ne peuvent être qu'en première position
! de la quotation.
! Exemple : un traitement d'exception doit avoir le même stack effect que le traitement normal

! ===== Les stack effects =====
! retourner * dit qu'on ne reviendra jamais




! ===== Les variables local =====

! Package locals

! Commencer la définition par :: bind les noms du stack effect sur les valeurs de la pile
:: delta ( a b c -- delta )
    b sq 4 a * c * -
;

! Autre version avec let :
: delta2 ( a b c -- delta )
    [ let
        :> c :> b :> a ! dépile et met dans c, puis dépile et met dans b, puis dépile et mets dans a
        b sq 4 a * c * -
    ]
;

! [| : retourne une quotation, mais n'exécute rien. Elle s'appliquera lorsqu'on l'appellera avec call
! il faudra donc (ici) qu'il y ait 3 éléments sur la pile au moment de l'appel à call
: delta3 ( a b c -- delta )
    [| a b c | b sq 4 a * c * - ]
;

1 2 3 delta3 3curry ! retournera -8 au moment de call => permet de binder les variables au moment de la déclaration




! Petite digression sur bi*
[ dip ] dip call
! x y p q
! x y p       q   Le premier dip
! x p         y q Le deuxième dip met le y de côté le temps d'appliquer p à x...
! p(x)        y q ... et applique la quotation p
! Retour des dip => ce qui était de côté revient
! p(x) y q


! Définition de dip : fait intervenir dip, mais dans un cas où le compilateur sait l'optimiser (quand 
! la quotation à exécuter est connue statiquement à la compilation)

! Dans le même genre : if fait appel à ?, et ? fait appel à if, mais transformant le code en quelque chose d'optimisable
! car suffisamment de choses sont connues statiquement



! ==================================================================
! ======================== LES CONTINUATIONS =======================

! Une variable qui contient toute la suite du programme, qui pourra être rappelé
! à tout moment

SYMBOL: k
[ k set ] callcc0 "toto" print
! callcc0 prend une quotation, l'exécute en lui passant sur la pile "le reste du programme à exécuter"
! On a donc mis la continuation dans k
!   -> Si on fait : "k continue", ça affichera toto

k get continue k get continue ! n'exécute la continuation qu'une fois, car une continuation ne revient pas.
! En effet, une continuation contient la fin du programme, ce serait donc absurde de ré-exécuter qqch après

[ k set 42 ] callcc1 .
17 k get continue-with ! exécutera le code "." en remplaçant 42 par 17
"toto" k get continue-with

! Intérêt par rapport à une fonction : l'état de toute la pile est aussi sauvé dans la continuation
