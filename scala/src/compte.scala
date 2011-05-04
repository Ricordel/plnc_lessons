//class CompteBanque [>...<] {

//Java way !
    //private def balance = InitialBalance
    //def getBalance : Int = balance
    //def setBalance(bal : Int) : Unit = balance = bal
//}

/* En scala, chaque var se voit associé un getter et un setter. le setter s'appelle <variable>_=, que l'on peut surcharger à volonté*/

class CompteBanque (initialBalance : Int) { // Déclare le constructeur directement ici
        
    def this() = this(0)  // Constructeur secondaire qui appelle le constructeur primaire (déf au-dessus).
                          // il est obligatoire de rappeler le constructeur primaire

    protected var balance = initialBalance

    /* Si on ne met pas de parenthèses à la déclaration, on n'a pas le droit non plus d'en mettre à l'exécution
    * Ca permet de faire passer un getter/setter pour un véritable champ */
    //def balance = myBalance


    def transfert(other : CompteBanque, montant : Int) = {
        require(montant > 0) // lève une exception si la condition est False
        require(balance >= montant)
        balance -= montant
        other.balance += montant
    }

    def main(args : Array[String]) = {
        compte = new CompteBanque(100)
        compte_bis = new CompteBanque(50)
        println("Montant compte 1 : " + compte.balance)
        println("Montant compte 2 : " + compte_bis.balance)
        compte.transfert(compte_bis, 20)
        println("Montant compte 1 : " + compte.balance)
        println("Montant compte 2 : " + compte_bis.balance)
    }
}
