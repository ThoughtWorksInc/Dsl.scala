enablePlugins(Example)

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.12" % Test

publishArtifact := false

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.6.17" % Optional cross CrossVersion.for3Use2_13
libraryDependencies += "com.thoughtworks.binding" %% "binding" % "13.0.0-M0" % Optional
libraryDependencies += "org.scala-lang.modules" %% "scala-async" % "1.0.1" % Optional cross CrossVersion.for3Use2_13 intransitive ()
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.0" % Optional
libraryDependencies += "com.twitter" %% "algebird-core" % "0.13.9" % Optional cross CrossVersion.for3Use2_13 intransitive ()
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.15.4" % Optional
// libraryDependencies += "com.thoughtworks.each" %% "each" % "3.3.4" % Optional cross CrossVersion.for3Use2_13
libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.3.0" % Optional
libraryDependencies += "io.monix" %% "monix" % "3.4.0" % Optional
libraryDependencies += "com.typesafe.akka" %% "akka-http" % "10.2.9" % Optional cross CrossVersion.for3Use2_13
libraryDependencies += "com.typesafe.akka" %% "akka-stream" % "2.6.19" % Optional cross CrossVersion.for3Use2_13 intransitive ()

import Ordering.Implicits._
sourceGenerators in Test := {
  (sourceGenerators in Test).value.filterNot { sourceGenerator =>
    VersionNumber(scalaVersion.value).numbers >= Seq(2L, 13L) &&
    sourceGenerator.info
      .get(taskDefinitionKey)
      .exists { scopedKey: ScopedKey[_] =>
        scopedKey.key == generateExample.key
      }
  }
}

import scala.meta._

exampleSuperTypes := exampleSuperTypes.value.map {
  case init"_root_.org.scalatest.freespec.AnyFreeSpec" =>
    init"_root_.org.scalatest.freespec.AsyncFreeSpec"
  case otherTrait =>
    otherTrait
}

exampleSuperTypes += init"_root_.org.scalatest.Inside"
exampleSuperTypes += init"_root_.com.thoughtworks.dsl.MockPingPongServer"
