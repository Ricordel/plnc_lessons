IN: bot_trust

USING: accessors grouping io kernel math math.parser math.order sequences splitting ;


TUPLE: robot position time min-time ;


! ========================= De quoi gérer une séquence d'ordres ===========================


! Donne un vecteur de couples (robot, bouton à atteindre)
: get-orders ( line -- vect )
    " " split rest 2 group
;


! Temps nécessaire au déplacement et à l'appui sur le bouton
: needed-time ( robot move -- time )
    [ [ time>> ] [ position>> ] bi ] dip
    second string>number
    - abs 1 + +
;


! De même en tenant compte du min-time dû au déplacement précédent
: earlier-possible ( robot move -- earlier-time-possible )
    [ needed-time ] [ drop min-time>> ] 2bi max dup number>string print
;


: do-move ( robot2 robot1 move -- updates_robot2 updated_robot1 )
    [ dup ] dip [ second string>number nip ] [ earlier-possible ] 2bi ! heure d'appui sur le bouton, et emplacement
    [ [ >>position ] dip >>time ] keep  ! mise à jour du robot qui a bougé
    swap [ 1 + >>min-time ] dip         ! mise à jour du robot qui n'a pas bougé
;
    

! Effectuer un mouvement
: step ( orange blue move -- updated_orange updated_blue )
    dup first "B" = [ do-move " le bleu bouge " print ] [ [ swap ] dip do-move swap " l'orange bouge" print ] if
;
    

! Effectuer tous les mouvements de la séquence
: all-steps ( orange blue moves -- final_orange final_blue )
    dup empty? [
        drop
    ] [
        [ first ] keep [ step ] dip
        rest all-steps
    ] if
;


! Temps minimum pour effectuer la séquence
: min-time ( orange blue moves -- min_time )
    1 0 0 robot boa swap
    1 0 0 robot boa swap
    get-orders
    all-steps
    [ time>> ] bi@ max
;



! ======================== Gérer tout le jeu de données =============================

: split-sequences ( string -- vector )
    "\n" split rest
;


: print-result ( -- )
    [ "Case #" >digit 1string append ": " append >digit 1string append print ] keep ! afficher le résultat
;

: all-seqs ( instructions nth -- )
    dup empty? [
        drop
    ] [
        [ [ rest ] [ first ] bi min-time ] [ 1 + ] bi* ! effectuer le calcul d'une séquence
        all-seqs
    ] if
;



: bot-trust ( string -- )
    split-sequences
;
