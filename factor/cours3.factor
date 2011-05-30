! Fry : construction facile de quotation

! On veut une f° qui multiplie pas 2 puis ajoute n
: func ( n -- quot )
    [ swap 2 * + ] curry
;


: func2 ( n -- quot )
    '[ 2 * _ + ] ;

! Attention : prend les quotations "dans leur ensemble" sur la pile. I.e : si on a 4 5 sur la pile, qu'une quot avec fry
! contient deux _, alors le premier prendra 4 et le 2è prendra 5

: func3 ( n -- quot )
    '[ _ 1 + ]
;


! Opérateur @ :
: func4 ( n -- quot )
    '[ @ 1 + ]
;

! Si on avait une quotation au sommet de la pile, elle a été "explosée" à l'intérieure de la nouvelle quotation
! C'est la même différence que [l1, l2] et l1 @ l2 en Caml

: mycurry ( obj quot -- quot' )
    ' [ _ @ ]
;

! Fry est implémenté en Factor et n'a rien de primitif dans le langage



! === L'opératuer iota ===
10 iota ! donnera 1..9 lorsqu'on iterera dessus

10 iota [ 2 * ] map ! retournera bien { 1..20 }



! ===================================================================
! ========              L'OPÉRATEUR ANGÉLIQUE               =========
! ===================================================================


{ 1 2 3 4 } amb
fail 
! Essaie le 1, et continue jusqu'à un éventuel fail, à ce moment là il remonte au dernier amb, et passe au suivant

ERROR: failure ;
M: failure summary drop "No more alternatives !" ;


SYMBOL: fail*

: fail ( -- * )
    fail* get [ failure ] or
    call ( -- * ) ! rappel : * veut dire qu'on ne revient jamais
;

: reset ( -- )
    f fail* set
;


[ "toto" print "foobar" throw ] fail* set
fail
! donnera bien "toto" "foobar"



! Appeler set-fail avec une quotation empile la quotation qui sera appelée
! la prochaine fois qu'on fera un fail
: set-fail ( quot: ( -- * ) -- )
    fail* get ! on récupère l'ancienne valeur de fail*
    [ fail* set call ] 2curry ! sur la pile : quot (old_fail)
    fail* set
;


: amb ( seq -- x )
    dup empty? [
        fail
    ] [
        [
            unclip
            [
                [ amb swap continue-with ] 2curry set-fail
            ] dip
        ] curry callcc1 ! on veut renvoyer une valeur
    ] if
;




{ 1 2 3 } amb { 4 5 6 } amb ! laissera toujours deux valeurs sur la pile

! Récupère le premier couple de somme 7
{ 1 2 3 } amb { 4 5 6 } amb 2dup + 7 = [ fail ] unless ! laissera 1 6 sur la pile
! Un fail donnera 2 5




! Maintenant, on voudrait récupérer toutes les valeurs qui marchent
: bag-of ( quot: ( -- x ) -- seq )
    [ V{ } clone ] dip
    [ call( -- x ) swap push fail ] curry
    [ ] bi-curry
    [ { t f } amb ] 2dip if ! Pour savoir si on est en phase de collecte ou d'affichage des résultats
;



[ { 1 2 3 } amb { 4 5 6 } amb 2dup + 7 = [ fail ] unless ] bag-of ! fera bien ce qui est attendu





! ================= Avec ça, on peut se faire le problème des 8 reines ====================

: conflict? ( q1 q2 -- ? )
    [ [ first ] bi@ - ]
    [ [ second ] bi@ - ] 2bi
    2dup [ abs ] bi@ - * * 0 =
;


: (queens) ( n k -- sol )
    dup 0 = [
        2drop { }
    ] [
        [
            1 - (queens)
        ] [
            [ iota amb 1 + ] [ 2array ] bi*
        ] 2bi

        [
            [ conflict? [ fail ] when ]
            curry each
        ] [
            suffix
        ] 2bi
    ] if 
;


: queens ( n -- sol )
    dup (queens)
;


[ 8 queens ] bag-of length ! retourne bien 92

! amb existe déjà en factor dans le vocabulaire backtrace




! ========================== Un petit jeu ===========================

CONSTANT: words V{
    { "the" "that" "a" }
    { "frog" "elephant" "thing" "tree" }
    { "walked" "swims" "grows" "extends" }
    { "slowly" "quickly" }
}


! On veut faire des phrases ou chaque mot commence par la dernière lettre du précédent
[ words [ amb ] map ] bag-of ! retournera toutes les possibilités


: same-letter? ( w1 w2 -- ? )
    [ last ] [ first ] bi* =
;

: correct-sentence ( seq -- ? )
    dup rest
    [ sam-letter ] 2all ! 2all zip 2 séquences et fait le test sur les couples, exactement ce qu'on veut ici !
;

: search ( -- str )
    words [ amb ] map
    dup correct-sentence? [ fail ] unless
;


! et alors :
[ search ] bag-of ; ! fera bien ce qu'on veut
