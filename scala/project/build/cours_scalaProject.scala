import sbt._

class TD1Project(info: ProjectInfo) extends DefaultProject(info) {

  val scalaToolsSnapshots = "Scala-Tools Maven2 Snapshots Repository" at
                            "http://scala-tools.org/repo-snapshots"

  val scalatest = "org.scalatest" % "scalatest" % "1.3"

  //val snapshots = "shapshots" at "http://scala-tools.org/repo-snapshots"

  //val releases = "releases" at "http://scala-tools.org/repo-releases"

  val lift_json = "net.liftweb" %% "lift-json" % "2.3"

  val dispatch = "net.databinder" %% "dispatch" % "0.8.1"

}
