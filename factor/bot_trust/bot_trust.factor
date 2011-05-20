IN: bot_trust

USING: accessors grouping kernel math math.parser math.order sequences splitting ;


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
    [ needed-time ] [ drop min-time>> ] 2bi max
;



: do-move ( robot2 robot1 move -- updates_robot2 updated_robot1 )
     2dup [ second string>number nip ] [ earlier-possible ] 2bi [ drop ] 2dip ! heure d'appui sur le bouton, et emplacement
     [ [ >>position ] dip >>time ] keep ! mise à jour de l'heure et de la positon du robot qui a bougé
     swap [ >>min-time ] dip ! mise à jour du min-time du robot qui n'a pas bougé
;
    
    


: step ( orange blue move -- updated_orange updated_blue )
    dup first "B" = [ do-move ] [ swap do-move swap ] if
;
    


: min-time ( orange blue moves -- final_orange final_blue )
    [ step ] map ! drop ? OU 2map ? ou curry ? FIXME
    [ time>> ] bi@ max
;


    


! Un robot fait une action :
! : min-time ( robot action -- updated-robot )
    

! : bot_trust ( string -- )
    ! 0 0 robot boa dup clone ! construction des robots
! ;
