IN: engrammes

USING: math math.primes math.primes.factors kernel hashtables sequences assocs strings vectors ;



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
        H{ { f 0 } } substitute
        [ prime-factors ] map
    ] when
; PRIVATE>



! On peut voir la représentation sortie de prime-factors comme un arbre,
! on agit alors récursivement en distinguant les noeuds internes des feuilles
<PRIVATE : factors>engramme ( vect -- engramme )
    dup vector? [
        [ factors>engramme ] map concat "(" ")" surround
    ] [
        48 + 1string
    ] if
; PRIVATE>



: >engramme ( nb -- engramme )
    prime-factors factors>engramme
;





! Pour l'opération de décodage, il semble y avoir des parseurs EBNF en factor !
