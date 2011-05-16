IN: hanoi

USING: kernel math vectors sequences strings io ;

! === Affichage d'un mouvement ===
: move ( a b -- str )
    [ 48 + 1vector ] bi@
    [ " vers " ] dip
    append append >string
;



! Etant donné deux nombres parmi 1, 2, 3, retourne le 3è
: other ( a b -- o ) 6 swap - swap - ;


! b' est le 3è parmi 1, 2, 3
: partial ( a b -- a b' ) [ dup ] dip other ;



: hanoi ( d a n -- )
    dup
    1 = [
        drop move print
    ] [
        1 - [ partial ] dip [ hanoi ] 3keep   ! calcul de hanoi n-1 entre d et other
        [ partial 2dup move print ] dip   ! Afficher le déplacement du "grand disque"
        [ swap partial swap ] dip hanoi   ! calcul de hanoi n-1 entre other et a
    ] if
;
