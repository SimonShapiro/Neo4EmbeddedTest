name := "Neo4EmbeddedTest"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "org.neo4j" % "neo4j" % "2.3.0"

//libraryDependencies += "org.json4s" % "json4s-native_2.11" % "3.3.0"

//libraryDependencies += "org.json4s" % "json4s-jackson_2.11" % "3.3.0"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"

libraryDependencies += "com.typesafe.play" % "play-json_2.11" % "2.5.0-M1"

libraryDependencies += "org.scala-lang.modules" % "scala-pickling_2.11" % "0.10.1"

libraryDependencies += "org.freemarker" % "freemarker" % "2.3.23"
