ThisBuild / scalaVersion := "2.13.12"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.13.12"


lazy val hello = (project in file("."))
  .settings(
    name := "ScalaWithCats2"
  )
