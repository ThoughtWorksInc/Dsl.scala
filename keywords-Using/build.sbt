libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.6-SNAP2" % Test

enablePlugins(Example)

import meta._
exampleSuperTypes := exampleSuperTypes.value.map {
  case ctor"_root_.org.scalatest.FreeSpec" =>
    ctor"_root_.org.scalatest.AsyncFreeSpec"
  case otherTrait =>
    otherTrait
}

scalacOptions ++= {
  import Ordering.Implicits._
  if (VersionNumber(scalaVersion.value).numbers < Seq(2L, 12L)) {
    // Enable SAM types for Scala 2.11
    Some("-Xexperimental")
  } else {
    None
  }
}

libraryDependencies += "com.thoughtworks.dsl" %%% "keywords-await" % "1.3.1" % Test
libraryDependencies += "com.thoughtworks.dsl" %%% "domains-task" % "1.3.1" % Test
