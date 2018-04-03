enablePlugins(Example)

publishArtifact := false

libraryDependencies += "org.scala-lang.modules" %% "scala-async" % "0.9.7"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.5.10"

libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % "7.2.20"

libraryDependencies += "com.twitter" %% "algebird-core" % "0.13.4"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.5"

libraryDependencies += "com.thoughtworks.binding" %% "binding" % "11.0.1"

libraryDependencies += "com.thoughtworks.each" %% "each" % "3.3.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0"

libraryDependencies += "io.monix" %% "monix" % "2.3.3"

import scala.meta._
exampleSuperTypes += ctor"_root_.org.scalatest.Inside"