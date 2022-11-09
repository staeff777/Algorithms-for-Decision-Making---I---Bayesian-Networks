ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.3"

lazy val root = (project in file("."))
  .settings(
    name := "untitled1"
  )

// this was too complicated for a simple examble.., has no visualisation
// libraryDependencies += "de.sciss" %% "topology" % "1.1.4" //LGPL https://github.com/sciss/topology

libraryDependencies ++= Seq(
  // graph library
  "org.jgrapht" % "jgrapht-core" % "1.5.1", //Eclipse Public License
  "org.jgrapht" % "jgrapht-io" % "1.5.1", //Eclipse Public License
  // graphviz for graph visualisation
  "guru.nidi" % "graphviz-java" % "0.18.1", //Eclipse Public License
  "guru.nidi" % "graphviz-java" % "0.18.1", //Eclipse Public License
  // testing
  "org.scalatest" %% "scalatest" % "3.2.12" % Test
)