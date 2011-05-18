IN: engrammes

USING: assocs combinators combinators.short-circuit kernel hashtables math math.functions math.parser math.primes math.primes.factors peg peg.ebnf sequences strings vectors ;




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
! premiers, de manière récursive (les exposants trop grands sont aussi transformés)
<PRIVATE : prime-factors ( nb -- list )
    dup 1 > [
        decompose
        [ at dup [ drop 0 ] unless ] curry map
        [ prime-factors ] map
    ] when
; PRIVATE>



! On peut voir la représentation sortie de prime-factors comme un arbre,
! on agit alors récursivement en distinguant les noeuds internes des feuilles
<PRIVATE : factors>engramme ( vect -- engramme )
    dup vector? [
        [ factors>engramme ] map concat "(" ")" surround
    ] [
        >digit 1string
    ] if
; PRIVATE>



: >engramme ( nb -- engramme )
    prime-factors factors>engramme
;





! ==========================================================
! ====              Décodage des engrammes              ====
! ==========================================================


ERROR: malformed ;

EBNF: engramme
    digit = [0-1] => [[ digit> ]]

    simple = (digit)+

    eng =  digit 
            | "(" simple ")" => [[ second ]]
            | "(" (eng)+ ")" => [[ second ]]
;EBNF


<PRIVATE : n-first-primes ( n -- list )
    [ V{ } clone 1 ] dip
    [ next-prime [ over push ] keep ] times drop
; PRIVATE>


! Pour calculer la valeur associée au vecteur retourné par le parseur,
! on calcule récursivement la valeur des divers exposants, ce qui nécessite
! de n'avoir qu'un seul niveau de profondeur dans la notation des exposants

<PRIVATE : compute-tree ( vector -- int )
    dup [ vector? ] any? [ ! Cas de récursion
       [ dup vector? [ compute-tree ] when ] map
       compute-tree
    ] [     ! Cas de base
        dup length n-first-primes zip ! tableau du même type que group-factors, mais les couples sont dans l'autre sens
        [ dup first [ second ] dip ^ ] [ * ] map-reduce
    ] if
; PRIVATE>



: engramme>number ( eng -- n )
    engramme
    dup vector? [ compute-tree ] [ ] if
;
