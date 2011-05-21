IN: bot_trust

USING: accessors grouping io kernel math math.parser math.order sequences splitting ;


TUPLE: robot position time min-time ;


! J'aimerais bien un vecteur de couples, chaque couple étant du type { "robot" bouton }
: get-orders ( line -- vect )
    " " split rest 2 group
;



: needed-time ( robot move -- time )
    [ [ time>> ] [ position>> ] bi ] dip
    second string>number
    - abs 1 + +
;



: earlier-possible ( robot move -- earlier-time-possible )
    [ needed-time ] [ drop min-time>> ] 2bi max dup number>string print
;



: do-move ( robot2 robot1 move -- updates_robot2 updated_robot1 )
    [ dup ] dip [ second string>number nip ] [ earlier-possible ] 2bi ! heure d'appui sur le bouton, et emplacement
    [ [ >>position ] dip >>time ] keep ! mise à jour du robot qui a bougé
    swap [ 1 + >>min-time ] dip ! mise à jour du robot qui n'a pas bougé
;
    
    


: step ( orange blue move -- updated_orange updated_blue )
    dup first "B" = [ do-move " le bleu bouge " print ] [ [ swap ] dip do-move swap " l'orange bouge" print ] if
;
    


: all-steps ( orange blue moves -- final_orange final_blue )
    dup empty? [
        drop
    ] [
        [ first ] keep [ step ] dip
        rest all-steps
    ] if
;


: min-time ( orange blue moves -- min_time )
    get-orders
    all-steps
    [ time>> ] bi@ max
;


    


! Un robot fait une action :
! : min-time ( robot action -- updated-robot )
    

! : bot_trust ( string -- )
    ! 0 0 robot boa dup clone ! construction des robots
! ;
