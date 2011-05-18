IN: engrammes

USING: assocs kernel hashtables math math.functions math.parser math.primes math.primes.factors peg peg.ebnf sequences strings vectors ;




! ==========================================================
! ====              Encodage des engrammes              ====
! ==========================================================




! Mettre sur la pile la liste des facteurs premiers et de leurs exposants respectifs
! sous la forme d'une hashtable, ainsi que la liste des facteurs premiers qui seront
! nécessaires
<PRIVATE : decompose ( nb -- factors_list primes_list )
    group-factors [ supremum first primes-upto ] keep >hashtable
; PRIVATE>



! Mettre sur la pile un vecteur contenant les exposants des divers nombres
! premiers, de manière récursive (les exposants trop grands sont aussi transformés
<PRIVATE : prime-factors ( nb -- list )
    dup 1 > [
        decompose
        [ at ] curry map
        H{ { f 0 } } substitute ! FIXME : 0|| devrait s'y prêter mieux ?
        [ prime-factors ] map
    ] when
; PRIVATE>



! On peut voir la représentation sortie de prime-factors comme un arbre,
! on agit alors récursivement en distinguant les noeuds internes des feuilles
<PRIVATE : factors>engramme ( vect -- engramme )
    dup vector? [
        [ factors>engramme ] map concat "(" ")" surround
    ] [
        48 + 1string ! TODO : il doit y avoir un truc built-in qui fait ça
    ] if
; PRIVATE>



: >engramme ( nb -- engramme )
    prime-factors factors>engramme
;






! ==========================================================
! ====              Décodage des engrammes              ====
! ==========================================================


EBNF: engramme
    digit = [0-1] => [[ digit> ]]

    simple = (digit)+

    eng =  digit 
            | "(" simple ")" => [[ second ]]
            | "(" (eng)+ ")" => [[ second ]]
;EBNF


: n-first-primes ( n -- list )
    [ V{ } clone 1 ] dip
    [ next-prime [ over push ] keep ] times drop
;


: compute-tree ( vector -- int )
    dup [ vector? ] any? [ ! Cas de récursion : il y a encore des niveaux dans l'arbre

       [ dup vector? [ compute-tree ] when ] map  ! réduit un étage
       compute-tree ! et recommence !

    ] [ ! Cas de base, il n'y a que des nombres dans le tableau
        dup length n-first-primes zip ! tableau du même type que celui donné par group-factors, mais à l'envers
        [ dup first [ second ] dip ^ ] [ * ] map-reduce
    ] if
;


: engramme>number ( eng -- n )
    engramme compute-tree
;


