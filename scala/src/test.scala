object Prog {
    def main(args : Array[String]) : Unit = {
        if (args.size == 0)
            println("Coucou !")
        else {
            val a = args(0)
            println("Argument : " + a)
        }
    }
}
