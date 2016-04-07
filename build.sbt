lazy val root = (project in file(".")).
  settings(
    scalaVersion := "2.11.7",
    organization := "com.lewisvail3",
    name := "scala-grep",
    version := "0.2.1-SNAPSHOT",
    
	libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.6" % "test"
  )
