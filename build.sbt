name := """Markov"""

version := "1.0"

scalaVersion := "2.11.1"

scalacOptions ++= Seq("-feature")

// Change this to another test framework if you prefer
libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.6" % "test"

libraryDependencies ++= Seq("NicMart" %% "weightedrandomdistribution" % "1.0")

//libraryDependencies ++= Seq("com.gravity" %% "goose" % "2.1.25-SNAPSHOT")

// Uncomment to use Akka
//libraryDependencies += "com.typesafe.akka" % "akka-actor_2.11" % "2.3.3"

resolvers += Resolver.sonatypeRepo("public")