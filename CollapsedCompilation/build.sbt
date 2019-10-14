import Dependencies._

showSuccess := false

logLevel in run := Level.Warn

libraryDependencies += "com.github.scopt" %% "scopt" % "3.7.0"
lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.10.6",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "compilation_test",
	javaOptions in run += "-Djava.library.path=./lib/"
  )
