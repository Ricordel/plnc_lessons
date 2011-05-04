/** Note : apprendre SBT ! **/


/* Scala est entièrement interopérable avec Java. Il existe des conversions implicites entre
* types Java et types Scala.
* Ex : les chaînes Scala sont des java.lang.String, toutes les méthodes Java sont donc disponibles
*
* Un point sur la hiérarchie des types : 
*      any --- anyval (Int, Double, ...)
*           |
*           -- anyref (Regexp, Object, ...)
*
*      Null : hérite de tous les types de Anyref, a une seule instance (null) qui correspond au null de Java.
*      Nothing : qui dérive TOUT, mais dont on ne peut pas créer d'instance. Utile par ex pour représenter des listes vides
*
* Read - execute - print loop (REPL).
*
* Pas d'opérateurs, les symboles "::", "->" sont en fait des appels de fonctions sur les bons types
*/


/* dictionnaires, listes */
list1 =  List(1, 2, 3)

/* Concaténation */
10 :: list1

/* Concaténer en queue : impossible comme ça, il faut concaténer deux listes => Attention O(n) */
list ::: List(4)

"foobar" :: 10 :: list1 // Ca marche -> liste de Any

/* Les Arrays : exactement comme en Java, taille immutable */
Array(1, 2, 3)

/* Les Maps : par défaut immutable, pas de modification sur place
* L'opérateur "->" s'applique à un type ArrayAssoc, il y a des transtypages implicites
* là-dessous.
*/
Map("Axel" -> "Schumacher", "Clément" -> "Moussu")


/* Appel de méthode sans argument */
list1 last


/* Il y a du pattern matching ! */
10 match {
        | case "a": Int => println("C'est a !")
        | case _ : => println("C'est pas a")
|}

/* Formatage de chaînes : comme en Python avec .format(), ou avec de la concat */



/** Cf mymain.scala pour un petit programme principal **/


/* Comme en C, les accolades sont optionnelles s'il n'y a qu'une instruction (une instruction
    * peut être un if elseif else */

    /* Scala est fonctionnel, en particulier supporte la curryfication */
    def fact(n: Int) {
        if (n < 3)
            n
        else
            n * fact(n-1)
    }



/* On n'est pas obligé d'indiquer le type de retour SAUF si :
 *      - on a une fonction récursive
 */

 /* Si on veut une méthode statique, on n'a qu'à créer un objet et mettre la fonction dedans */

/* Par défaut, tout est public. On peut ajouter du private, et même préciser le scope */


/** Utilisation de types Java, ex : lire le répertoire courant **/
res = new java.io.File(".")
res.listFiles
res.map(_, getName)
res.filter(_.endsWith(".txt"))

/* Val et Var : 
 *      Val pour une constance (immutable)
 *      Var pour une variable (mutable)
 *
 * Attention : typage fort. On va automatiquement dans le type "le plus bas" si on ne précise pas le type
 */
 val a = 3
 var b = 2 // Int inféré
 b = "foobar" // Erreur !

 var c: Any = 5 // Type Any
 c = "Coucou" // Fonctionne !


/** Surcharge : DOIT être indiquée avec le mot clé override **/


/* Appel de fonction f(1, 2) : appelle la méthode apply de f : f.apply(1, 2) */

/* autre ex : */
List(1, 2, 3)
/* appelle list.apply(1, 2, 3) qui fait un new */

/* Les comparaisons : 
 *      == n'est pas le == de Java (qui compare les références) mais un appel à la méthode equals()
 *      qui compare les valeurs. Par ex, une comparaison de listes s'effectue valeur par valeur.
 *      C'est bien la méthode equals() qu'il faut surcharger si besoin
 *
 * A ce propos : utilisatino des case-classes qui définissent des trucs sympas comme une comparaison
 * à base de hash de tous les champs
 */


/* Ex de pattern matching, qui utilise impliticement unapply() */
x.match
        | case List(y) => println("x = [" + y + "]")
        | case _ => println("Pas une liste d'un élément")
