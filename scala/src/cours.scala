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
         case List(y) => println("x = [" + y + "]")
         case _ => println("Pas une liste d'un élément")




/** Le pattern matching **/
val persons = List(Person("John", "Doe"), Person("victor", "Perron"), Person("truc", "bidule"))

persons foreach (_ match {
    case Person(first, last) if first.startswith("J") => println(last)
    case x => println("No match for " + x)
})


/* Si on veut afficher l'objet lui-même : on peut recréer un objet Personne
 * mais c'est moche. Mieux : */

persons foreach (_ match {
    case p @ Person(first, last) if first.startswith("J") => println(p) // p est l'objet Personne
    case x => println("No match for " + x)
})

Some(3) match {
    case Some(x: Int) => println(x)
    case _ => ()
}
/* Remplacer le début par Some("foobar"), le compilateur sait statiquement que le match
 * n'a pas de sens, et refuse de compiler */
 



for (x : Int <- Some(3)) println(x)
/* Fonctionne car for revient à appeler des map, foreach, ..., qui "déshabillent le Some" */

for (Person("Jonh", last) <- persons) println(last) // Le for a fait un pattern matching tout seul

/* Plus loin : ne fait rien cas la Map est vide => retourne un None => ne fait rien
 * Si la Map n'est pas vide mais ne contient pas les infos pour tout le monde, ça n'affiche
 * que ceux dont l'âge est entré, car sinon il y a un None dans la chaine => continue; */
for (p @ Person("Jonh", last) <- persons; age <- (Map[String, Int]()).get(last)) println(last)


/** Créer son itérateur **/
def js = for (p @ Person("Jonh", last) <- persons; age <- (Map[String, Int]()).get(last)) yield(p)
for (j <- js) println(j)



/** Nos Some et None, mieux que la dernière fois **/

/* En faisant ça, on profite du fait que getters et setters sont créés automatiquement
 * dans les case-class pour surcharger la méthode get() du type O */
case class S[T] (override val get: T) extends O[T] {
    val isEmpty = false
}


/** Types paramétrés **/
val s : S[Any] = (S(3) : S[Int]) // ne marche pas et ne doit pas marcher


/******************* Covariance et contravariance ***************************/

/* Introduire de la covariance de type : paramétrer par [+T] au lieu de [T] =>
 * O[B] est maintenant un enfant de O[A] si B est un enfant de A */


/* Un cas de mauvaise utilisation de la covariance => le compilo râle, et dit que
* le type covariant est en position contravariante (= inverse de covariant)
* Pour se mettre en mode contravariant, on met un - à la place du + */
abstract class OC[+T] {
    def send(x : T) : Unit
}


/* Ni coraviant, ni contravariant */
case class Var[T] (var x : T)

/* Covariant : */
case class Var[+T] (var x : T)
/* Ne marche pas : le setter d'un Var[Int] par exemple, ne peut prendre QUE un Int
 * donc la covariance ne convient pas */

/* Contravariant : */
case class Var[-T] (var x : T)
/* Ne marche pas non plus, mais cette fois c'est le getter qui pose problème, lui est dans
 * une position covariante */


/* Que peut-on faire avec ça ? */
 def f(a : A) : B

 def f(a : A1) : B // A1 dérive de B => ne marche pas, position contravariante : on n'est plus capable de prendre n'importe quel A
 def f(a : Any) : B // OK
 def f(a : A) : Any // Non => problème inverse
 def f(a : A) : B1 // OK => on retourne un B1, donc effectivement un B, on n'a jamais dit qu'on devait être surjectif sur B



/***** Écrivons notre classe pour remplacer les listes *****/
abstract class L[+T] {
    def ::[U >: T] (elem : U) : L[U] = Link(elem, this) // Attention, les opérateurs finissant en : appellent la méthode de l'élément de droite
}

/* La notation >: T veut dire "ancêtre" (au sens large). => U sera alors un type commun au type qu'on veut
 * insérer, et à T. On prend le plus proche ancêtre commun 
 * Il existe pareil pour U <: T, qui veut dire U dérive de T. C'est souvent implicite, mais peut être utile */

case class Link[+T] (val head : T, val tail : L[T]) extends L[T]

case object E extends L[Nothing]






