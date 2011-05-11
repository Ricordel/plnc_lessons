/*******************************************************************************
 * Un programme qui interroge l'API search.twitter.com
 *******************************************************************************/


import scala.actors.Actor
import scala.actors.Future



/* Représente un tweet, utile pour l'extraction automatique depuis le JSON */
case class Tweet(val from_user: String, val text: String, val created_at: String)
{
    override def toString() = "\n" + from_user + " wrote : \n" + text
}

/* Classes à utiliser pour envoyer des requêtes à l'acteur */
case class Query (val q: String)
case class LimitedQuery (val q: String, val limit: Int)

/* A passer à l'acteur pour qu'il se termine */
object Exit


object TwitterActor
{



    def main(args : Array[String]) =
    {
        /** On peut passe comme arguments : 
         *      - une requête
         *      - optionnellement un nombre maximum de tweets à récupérer
         */

        if (args.size < 1)
            exit()

        /** Les requêtes passent pas l'intermédiaire d'un acteur **/
        val actor = new TwitterActor
        actor.start

        val ret =
            if (args.size == 1)
                actor !! (Query(args(0)), { case t: List[Tweet] => t} )
            else
                actor !! (LimitedQuery(args(0), args(1).toInt), { case t: List[Tweet] => t })

        ret() match {
            case l: List[Tweet] =>
                if (l.length == 0)
                    println("IL n'y a pas de tweet correspondant")
                else {
                    println("\n\nVoici les " + l.length + " derniers tweets correspondant à la recherche " + args(0) + "\n")
                    l foreach println
                }
        }

        println("\n\n")
        actor ! Exit
    }




}



/** Toutes les requêtes passent par cet acteur, pour éviter d'effectuer plusieurs requêtes
 * en parallèle (même si ça ne peut pas arriver dans ce petit programme **/
class TwitterActor extends Actor
{
    import net.liftweb.json._
    import dispatch._
    /* Format implicite utilisé par les méthodes de la blibliothèque JSON */
    implicit val formats = DefaultFormats


    override def act() = 
        loop {
            react {
                case Exit => exit()
                case Query(q) => reply(search(q, -1))
                case LimitedQuery(q, n) => reply(search(q, n))
                case _ => reply(error("Le message doit être Exit, Query(String) ou LimitedQuery(String, Int)"))
            }
        }


    /** Cette fonction va chercher sur Twitter les "limit" derniers
     * tweets correspondant à la requête req, à l'aide de la bibliothèque dispatch.
     * Si limit == -1, c'est la valeur par défaut de Twitter (apparemment 15) qui est utilisée.
     * Comme on veut que toutes les requêtes passent par l'acteur au lieu d'être faites
     * directement, cette méthode est privée **/
    private def search(req: String, limit: Int) : List[Tweet] =
    {
        val http = new Http

        val request = 
            if (limit == -1)
                :/("search.twitter.com") / "search.json" <<? Map("q" -> req)
            else
                :/("search.twitter.com") / "search.json" <<? Map("q" -> req, "rpp" -> limit.toString, "p" -> "1")
        
        val raw_rslt = parse(http(request as_str)) \ "results"
        raw_rslt.children map { _.extract[Tweet] }
    }

}
