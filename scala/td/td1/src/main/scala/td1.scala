package net.rfc1149.td1

object TD1 {

        /** Exercice 1 **/
        def isOdd(n : Int) = (n % 2 == 1)
        def isEven(n : Int) = (n % 2 == 0)


        /** Exercice 3 **/
        def myWhile(b : => Boolean, f : => Any) : Unit = {
                if (b) {
                        f
                        myWhile(b, f)
                }
        }


        /******************* Problème des reines ******************/
        /* Définissons un alias de type */
        type Position = (Int, Int)

        /** Safe si elles ne sont pas sur la même ligne, même colone, ou même diagonale **/
        def safe(q1: Position, q2 : Position) : Boolean =
                q1._1 != q2._1 && // Ligne
                q1._2 != q2._2 && // Colone
                (q1._1 - q2._1).abs != (q1._2 - q2._2).abs // Diagonale


        def solveQueens(numberOfQueens: Int, f : List[Position] => Unit) : Unit = {
            
            /** Résolvons le problème de manière récursive, pour placer n reines sur l'échiquier
             * numberOfQueens * n, il suffit d'en placer n-1 sur l'échiquer numberOfQueens * (n-1),
             * puis d'en ajouter une dans une nouvelle colone en évitant les positions dangereuses **/
            def place(n: Int) : Iterable[List[Position]] = {
            // Iterable[List] car il y a plusieurs solutions, Iterable pour transformer ça en générateur
                if (n == 0)
                    List(List())
                else
                    for {
                        queens <- place(n-1) // Pour chaque solution trouvée pour n-1
                        column <- 1 to numberOfQueens // Pour chaque colone de l'échiquier (bcles imbriquées !)
                        current = (n, column)
                        if queens.forall(safe(_, current)) // et si la place est sûre
                    }
                        // Alors on a une nouvelle solution (que l'on yield au lieu de concaténer car on retourne un itérable)
                        yield current :: queens // Ceci est le "corps de la boucle for"

            }

            // Effectuer l'action désirée sur le résultat final
            place(numberOfQueens).foreach(f)

        }

}
        



object td1 extends Application {

        import TD1._

        // Placer ici le code à exécuter

}


/** Exercice 2 **/
class ExtSeq[T] (sequence : Seq[T]) {

        private val seq : Seq[T] = sequence

        /* La version "comme en Caml" */
        /*
        def all(f : T => Boolean) = {
                def aux (fun : T=> Boolean, s : Seq[T]) : Boolean = {
                        s match {
                                case Nil => true
                                case head::queue => fun(head) && aux(fun, queue)
                        }
                }
                aux (f, seq)
        }
        */


        /* La version "utilise les trucs pratiques de Scala" */
        def all(f : T => Boolean) = seq.forall(f)

        /* IL y en a au moins un de vrai s'ils ne sont pas tous faux, donc pas tous
         * vrais pour le test complémentaire */

        //def any(f : T => Boolean) = !all(!f) // Ne compile pas...
        def any (f : T => Boolean) = {
                def not_f(x : T) = !f(x)
                !seq.forall(not_f)
        }
        
}




object ExtSeq {

        /* La conversion implicite pour les séquences "normales" */
        implicit def toExtSeq[T](s : Seq[T]) = new {
                def all(f : T => Boolean) = (new ExtSeq[T](s)).all(f)
                def any(f : T => Boolean) = (new ExtSeq[T](s)).any(f)
        }

}





/** Exercice 3 **/
class ExtCond (bool :  => Boolean) {

        private def b = bool

        def doWhile(f : => Any) : Unit = {
                if (b) {
                        f
                        doWhile(f)
                }
        }
}


object ExtCond {
        implicit def toExtCond(b : => Boolean) = new {
                def doWhile(f : => Any) = (new ExtCond(b)).doWhile(f)
        }
}






/***************************************************************
 *              Exercice 4 : nombres complexes
 ***************************************************************/
case class Complex(a : Double, b : Double)
{
        import Complex._
        import scala.math

        val re : Double = a
        val im : Double = b

        /** Un pretty print **/
        override def toString() = {
                if (im == 0)
                        "" + re
                else if (re == 0)
                        "" + im + "i"
                else if (im < 0)
                        "" + re + "-" + im.abs + "i"
                else
                        "" + re + "+" + im + "i"
        }

        
        def reciprocal() = Complex(a, -b)

        def +(other : Complex) = Complex(other.re + re, other.im + im)

        def unary_- = Complex(-re, -im)

        def -(other : Complex) = this + (-other)

        def *(other : Complex) = {
                val real = re*other.re - im*other.im
                val imag = re*other.im + im*other.re
                Complex(real, imag)
        }

        def /(other : Complex) = {
                require(other != 0)
                val a = other.abs
                val real = (re*other.re + im*other.im) / (a*a)
                val imag = (im*other.re - re*other.im) / (a*a)
                Complex(real, imag)
        }

        def abs() = math.sqrt(re*re + im*im)

        override def equals(other : Any) =
                other match {
                        case c : Complex => (re == c.re && im == c.im)
                        // Aucune conversion implicite n'est tentée, on ajoute donc explicitement
                        // la comparaison d'égalité avec un entier ou un flottant
                        case d : Double => re == d
                        case n : Int => re == n
                        case _ => false
                }

}




object Complex
{
        // Inutile car Complex est une case-class, ceci est déjà défini
        //def apply(re : Double, im : Double) = new Complex(re, im)

        def apply(re : Double) = new Complex(re, 0)


        // Il doit déjà y avoir des conversions implicites Int => Double,
        // et le tout s'enchaîne.
        // NON : règle de one-at-a-time : on ne chaîne pas les conversions implicites
        // le fait que ça marche viendrait-il alors du fait que 3 est une écriture
        // valable pour un Double ?
        implicit def toComplex(x : Double) = Complex(x, 0)
}
