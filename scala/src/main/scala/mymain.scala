/* Créé en fait un singleton */
object MyProgram {
    def main(args: Array[String]) : Unit =  {
        println("Programme principal")
        if (args.size != 1)
            error("Pas le bon nombre d'arguments")
        //require(args.size == 1) // équivalent, lève une exception
        val a : Int = args(0).toInt
        println("fact(%d) = %d".format(a, fact(a)))
    }

    def fact(n : Int) : Long = {
        if (n < 3)
            n
        else
            n * fact(n-1)
    }

}
