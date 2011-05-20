IN: plnc2011.intro

USE: sequences
USE: kernel
USE: math
! Mieux : 
USING: kernel math sequences unicode.case memoize ;

: toto ( -- s ) "toto" ;
: mean ( list -- mean ) [ sum ] [ length ] bi / ;

: palindrome? ( s -- ? ) [ alpha? ] filter >lower 
        dup reverse = ;


: empty ( -- v ) V{ } ;
1 over push
! Cela vient de modifier l'objet vecteur vide qui a été généré
! une seule fois à la compilation !

! Pour que ça ne pose pas de problème : 
: empty ( -- v ) V{ } clone ;

: global ( -- v ) V{ 0 } ;
: read ( -- v ) global last ;
: write ( v -- ) global over push ; ! ??
: write ( v -- ) global push ; ! sera mieux !


! Des hashtables :
H{ { "Bertrand" "Chazot" } { "John" "Doe" } } ;
! Regarder dans le vocatulaire assocs, par exemple "assocs
! about"

[ >upper ] change-at ; ! Attention à l'ordre des éléments de la
! entre la hashmap et la clé, la map doit être au-dessus de la clé

[ [ >lower ] change-at ] keep ; ! permet de garder la map
! Attention, comme les Vectors, les tables de hashage sont
! mutables !




! Les tuples (on commence à faire de l'objet)
TUPLE: customer balance beverages ;
TUPLE: drink name cost ;

C: <drink> drink  ! Défini un constructeur "automatique"

: <customer> ( -- e )
        customer new 0 >>balance V{ } clone >>beverages ;

        ! Pour voir la valeur de balance, si on a un customer :
        dup balance>> ! Dup car ça consomme un élément de la pile


: add-drink-to-beverages ( drink customer -- )
        beverages>> push 
        ! Car beverages est un vector, donc mutable
        ! beverages>> comsomme un customer et empile son champ beverages


! Créer une nouvelle boisson (grâce au constructeur automatique j'imagine ?
T{ drink f "long island" 4 } ;

! Acheter une boisson
: buy ( drink customer -- )
        [ add-drink-to-beverages ]
        [ pay ]
        2bi ;

! étapes sur la pile
! drink customer [A] [B]
! cost customer [B]

! Essayons de raccourcir (les mots doivent rester courts)
! Pour refactoriser, comme c'est concaténatif, il suffit 
! de recopier le bloc de code dans la déf du nouveau mot,
! et d'appeler le nouveau mot à l'emplacement où se trouvait
! le mot refactoriser.
 
! Les mots doivent être très courts, et le code doit
! s'auto-documenter sur ce qu'il fait.

! pay est générique car elle va dépendre du type
GENERIC: pay ( drink customer -- )

! La définition de pay pour un customer
M: customer pay ( drink customer -- )
        [ cost>> ]
        [ [ swap - ] change-balance drop ]
        bi*
;

! Et celle pour le barman
M: barman pay ( drink customer -- )
        [ cost>> ]
        [ [ swap 2 / - ] change-balance drop ]
        bi*
;

! Pour le faire plus joli, on pourrait faire une méthode
! discount qui prend en argument un coût et un customer
! et qui retourne le coût effectif de la boisson en fonction du
! type du mec qui consomme. Du coup, pay n'est plus générique,
! mais discount l'est


! Un barman doit avoir des réduction => héritage !!
! La définition
TUPLE: barman < customer ;
! Le constructeur :
: <barman> ( -- b )
        barman new 50 >>balance V{ } clone >>beverages ;




GENERIC: discout ( customer -- discount )

M: customer discount drop 1 ; ! drop car on n'en a rien à faire
! du customer, on veut juste son type.
M: barman discount drop 1/2 ;
: charge ( cost customer -- )
        [ swap - ] change-balance drop ] ;

: pay ( drink customer -- )
        [ cost>> ] [ [ discount * ] keep charge ] bi* ;

! une autre version :
dup [ discount * ] [ charge ] bi*
! utilise le fait qu'avec un bi*, lorsque la première fonction a
! été exécutée, le résultat est à sa place sur la pile



! Et un danceur en plus
TUPLE: dancer < customer ;
: <dancer> ( -- d )
        dancer new 0 >>balance V{ } clone >>beverage

M: dancer discount drop 2 ;




! Les prédicats : définir de nouveaux types d'objets sans
! définir de nouveaux types d'objets

! Ex sur les barmens :
: <barman> ( name -- b )
        [ barman new ] dip >>name 50 >>balance V{ }
        clone >>beverages ;


! Penser à virer ce putain de retour à la ligne automatique dans
! vim 


PREDICATE: president < barman name>> "Félix" = ;
! a créé automatiquement un mot president? pour savoir
! si un barman est président ou non

! On peut même s'en servir pour surcharger des méthodes :
M: president discout drop 0 ;
! Ca marche, la vérification est faite dynamiquement, mais
! on n'a pas vraiment créé de nouveau type


! Les conditions : 
! test [ si true ] [ si false ] if


! La suite de fibonacci
MEMO: fibo ( n -- x )
        dup 2 < [ drop 1 ] [ 1 - dup fibo swap 1 - fibo + ] if ;

! MEMO permet de se souvenir des anciens réusltats => met fibo
! en O(n), au lieu d'O(exp(n))
! Il y a un package à importer !

10 iota ! itérateur
[ fibo ] map
