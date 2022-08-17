ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.3"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.8.0"
libraryDependencies += "com.github.julien-truffaut" %% "monocle-core" % "3.0.0-M6"

lazy val root = (project in file("."))
  .settings(
    name := "Strategy"
  )
