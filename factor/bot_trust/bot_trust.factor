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

    

! : make-move ( orange blue move -- orange blue )
    


! Un robot fait une action :
! : min-time ( robot action -- updated-robot )
    

! : bot_trust ( string -- )
    ! 0 0 robot boa dup clone ! construction des robots
! ;
