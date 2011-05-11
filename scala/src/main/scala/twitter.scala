/*******************************************************************************
 * Un wrapper Scala pour une partie de l'API flickr
 *
 *******************************************************************************/

package net.ricordel.twitterApi

import scala.actors.Actor


/** Format implicite, utilisé par les méthodes de la lib **/

//parse(...).extract[Type]


case class Tweet(val from_user: String, val text: String, val created_at: String)
{
    override def toString() = "\n\n" + from_user + " wrote : \n" + text
}



// Penser à encoder la chaîne de recherche, avecn java.net.URL(..., "UTF-8")
object TwitterActor {
    import net.liftweb.json._
    import scala.actors.Actor
    import scala.io.Source._
    import dispatch._

    implicit val formats = DefaultFormats


    // Utiliser dispatch pour voir
    def search(req: String) : List[Tweet] = {
        val http = new Http
        val request = :/("search.twitter.com") / "search.json" <<? Map("q" -> req)
        
        val raw_rslt = parse(http(request as_str)) \ "results"
        raw_rslt.children map{_.extract[Tweet]}
    }



    def main(args : Array[String]) = {
        val actor = new TwitterActor
        actor.start
        val ret = actor !! "#fail" // Retourne une future

        ret() match {
            case l: List[Tweet] => l foreach println
            case _ => println("The answer was not a tweets list")
        }

        actor ! Exit
    }

}





class TwitterActor extends Actor
{
    import TwitterActor._

    
    def act() = 
        loop {
            react {
                case Exit => exit()
                case s: String => reply(search(s))
                case _ => reply("Message content must be a String")
            }
        }

}


object Exit
