ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.2"

lazy val root = (project in file("."))
  .settings(
    name := "crdt_playground",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.11" % "test",
    libraryDependencies += "org.scalatestplus" %% "scalacheck-1-15" % "3.2.11.0" % "test"
  )
