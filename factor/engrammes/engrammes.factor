IN: engrammes

USING: math math.primes kernel ;


! Utiliser du map-reduce : 
!   - map et les appels récursifs pour obtenir la structure logique
!   - reduce : en gros concaténation et pretty-print

! !!! Ajout des zéros ? !!!

! Surround pour ajouter les parenthèses
! regarder glue aussi

! supremum pour le max d'une liste

! at dans une assoc avec une valeur par défaut : existe
! ou utiliser le fait ue ça retourne f en cas d'erreur => truc || f fera ce qu'on veut
! curry
! Un map tout seul à la fin ??

! primes-upto

! Commencer par générer la liste des exposants des facteurs premiers jusqu'au plus grand
! dont on a besoin => on a généré les 0 qui nous manquaient.
! => appels récursifs là-dessus avec la pretty-print

! Regarder le combinateur case pour la fonction parse finale qui regroupe tout

! map ??



: parse ( int -- str )
    group-factors 
;


! Pour l'opération de décodage, il semble y avoir des parseurs EBNF en factor !
