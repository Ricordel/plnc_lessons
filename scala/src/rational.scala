/** Une classe avec des rationnels **/
class Rational(n : Int, d : Int) {

    require(d != 0)

    private val g = pgcd(n.abs, d.abs)

    /* Le mot-clé lazy permet de ne calculer les valeurs que si on en a besoin. Un peu plus lourd
    * au niveau stockage et exécution, mais utile si le calcul est rarement utilisé et couteux
    * en ressource */
    lazy val numerator = n / g * (if (d < 0) -1 else 1)
    lazy val denominator = d / g * (if (d < 0) -1 else 1)

    override def toString = 
        if (denominator == 1)
            numerator.toString
        else
            numerator + "/" + denominator // override indique la surcharge : obligatoire

    def +(r : Rational) : Rational = 
        Rational(numerator * r.denominator + denominator * r.numerator, denominator * r.denominator)
    /* Ici aussi on pourrait définir une méthode apply(), qui permettrait de faire un appel sur l'objet */

}



/* Objet compagnion, porter le même nom permet de partager des privilèges */
object Rational {

    def pgcd(a : Int, b : Int) : Int = 
        if (b == 0)
            a
        else
            pgcd(b, a%b)

    def apply(n : Int, d : Int) = new Rational(n, d)

    def apply(n : Int) = new Radional(n, 1)

}


class RationalSpec extends Specification {
    "A rational" should {
        "print proprerly" in {
            Rational(1, 2).toString
            /* ... */
