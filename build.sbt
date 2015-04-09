name := "StackExchangeAnalysis"

version := "1.0"

scalaVersion := "2.10.5"

mainClass := Some("main.Main")

libraryDependencies ++= Seq(
  "org.json4s" %% "json4s-native" % "3.2.11",
  "org.neo4j" % "neo4j" % "2.2.0"
)