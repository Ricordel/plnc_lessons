IN: factorielle

USING: math kernel ;


! ======================== Échauffement : la factorielle =============================

: fact ( n -- f )
        dup 0 <= [ drop 1 ] [ dup 1 - fact * ] if
;
