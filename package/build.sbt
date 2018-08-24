enablePlugins(Example)


libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.6-SNAP1" % Test

publishArtifact := false

libraryDependencies ++= {
  import Ordering.Implicits._
  if (VersionNumber(scalaVersion.value).numbers >= Seq(2L, 13L)) {
    Nil
  } else {
    Seq(
      "org.scala-lang.modules" %% "scala-async" % "0.9.7",
      "com.typesafe.akka" %% "akka-actor" % "2.5.10",
      "com.twitter" %% "algebird-core" % "0.13.4",
      "com.thoughtworks.binding" %% "binding" % "11.0.1",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0",
      "org.scalacheck" %% "scalacheck" % "1.13.5",
      "com.thoughtworks.each" %% "each" % "3.3.1",
      "com.lihaoyi" %% "sourcecode" % "0.1.4",
      "io.monix" %% "monix" % "2.3.3"
    )
  }
}

sourceGenerators in Test := {
  (sourceGenerators in Test).value.filterNot { sourceGenerator =>
    import Ordering.Implicits._
    VersionNumber(scalaVersion.value).numbers >= Seq(2L, 13L) &&
    sourceGenerator.info
      .get(taskDefinitionKey)
      .exists { scopedKey: ScopedKey[_] =>
        scopedKey.key == generateExample.key
      }
  }
}

libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % "7.2.25"

import scala.meta._
exampleSuperTypes += ctor"_root_.org.scalatest.Inside"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7")
