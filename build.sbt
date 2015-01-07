name := """Markov"""

version := "1.0"

scalaVersion := "2.11.1"

scalacOptions ++= Seq("-feature")

// Change this to another test framework if you prefer
libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.6" % "test"

libraryDependencies ++= Seq("NicMart" %% "weightedrandomdistribution" % "1.0")

libraryDependencies ++= Seq("com.gravity" %% "goose" % "2.1.25-SNAPSHOT")

// Uncomment to use Akka
//libraryDependencies += "com.typesafe.akka" % "akka-actor_2.11" % "2.3.3"

resolvers += Resolver.sonatypeRepo("public")

libraryDependencies  ++= Seq(
  // other dependencies here
  "org.scalanlp" %% "breeze" % "0.10",
  // native libraries are not included by default. add this if you want them (as of 0.7)
  // native libraries greatly improve performance, but increase jar sizes.
  "org.scalanlp" %% "breeze-natives" % "0.10"
)

resolvers ++= Seq(
  // other resolvers here
  // if you want to use snapshot builds (currently 0.11-SNAPSHOT), use this.
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)