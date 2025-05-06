ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.16"

lazy val root = (project in file("."))
  .settings(
      name := "y7km",
      libraryDependencies += "org.typelevel" %% "cats-effect" % "3.5.4"
  )
