import sbt._

class TD1Project(info: ProjectInfo) extends DefaultProject(info) {

  val scalaToolsSnapshots = "Scala-Tools Maven2 Snapshots Repository" at
                            "http://scala-tools.org/repo-snapshots"

  val scalatest = "org.scalatest" % "scalatest" % "1.3"

}
