/*******************************************************************************
 * Un wrapper Scala pour une partie de l'API flickr
 *
 *******************************************************************************/

package net.ricordel.twitterApi




/** Format implicite, utilisé par les méthodes de la lib **/

//parse(...).extract[Type]


object Twitter {

    def main(args : Array[String]) = {
        val proxy = new twitterProxy()
        proxy.first_test()
    }

}



object twitterProxy {
    implicit val formats = DefaultFormats
}



// Penser à encoder la chaîne de recherche, avecn java.net.URL(..., "UTF-8")
object twitterProxy {
    import net.liftweb.json._
    import net.liftweb.json.jsonParser._
    import scala.actors.Actor
    import scala.io.Source._

    def first_test()  = {
        val url = "search.twitter.com/search.json?q=@rfc1149&rpp=10&page=1"
        val rslt = parse(fromUrl(url))

        println(rslt)
    }

}


