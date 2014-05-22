name := "StreamAutomaton"

version := "1.0"

scalaVersion := "2.10.4"
//libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test"

resolvers ++= Seq(
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases",
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
)

libraryDependencies ++= Seq(
  //  "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2",
  //  "ch.qos.logback" % "logback-classic" % "1.1.2",
  "org.scalacheck" %% "scalacheck" % "1.11.4" % "test",
    "com.typesafe.akka" %% "akka-actor" % "2.3.3"
//  "com.typesafe.akka" % "akka-actor_2.10" % "2.4-SNAPSHOT"
  // "org.eintr.loglady" %% "loglady" % "1.1.0"
)

