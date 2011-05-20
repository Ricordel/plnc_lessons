IN: hanoi

USING: io kernel math math.parser sequences strings vectors ;


! === Affichage d'un mouvement ===
: move ( a b -- str )
    [ >digit 1vector ] bi@
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
        1 - [ partial ] dip [ hanoi ] 3keep   ! hanoi n-1 entre d et other
        [ partial 2dup move print ] dip   ! déplacement du "grand disque"
        [ swap partial swap ] dip hanoi   ! hanoi n-1 entre other et a
    ] if
;
