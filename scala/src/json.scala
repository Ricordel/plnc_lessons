/*************************** Faire du JSON avec scala ***************************/
/* Ajouter du lift_json dans le paramétrage du projet */
/* val lift_json = "net.liftweb" %% "lift-json" % "2.3" */

import net.liftweb.json._
import net.liftweb.jsonParser._


def f(implicit toto : Int) = println(toto)
implicit val x = 3 // Si c'est le seul implicit Int dans le scope => paramètre automatique
                   // si on appelle f sans argument               

/* Le parseul JSON est codé pareil : il y a un defaultFormat */
implicit val formats = DefaultFormats


// Si el type spécifié à extract convient bien à l'objet passé, 
// tout est automatique. S'il y a des champs en trop, ils sont ignorés
parse("""{"first" : "Jonh", "last" : "Doe"}""").extract[Person]

// bien sûr, il y a pareil pour sérialiser
import net.liftweb.json.Serialisation.write
write(Person("Sam", "Tardieu")) // va écrire l'objet JSON correspondant


// Faire la requête à l'API : utiliser les lib Java qui vont bien
// ou bien scala.io.Source.fromURL. Il existe aussi fromFile()
// Puis on peut utiliser split(), getLines(), ...

import scala.xml._
/* Maintenant il y a une tonne de trucs implicites super pratiques */
val x = <person><first>Jonh</first><last>Doe</last></person>
x \ "first" // retourne une séquence de noeuds

(x \ "first") text // Retourne John

(x \ "_") text // Retourne JonhDoe
