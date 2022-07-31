libraryDependencies += "com.typesafe.akka" %% "akka-http" % "10.2.9" % Test cross CrossVersion.for3Use2_13

libraryDependencies += "com.typesafe.akka" %% "akka-actor-typed" % "2.6.19" % Test cross CrossVersion.for3Use2_13

libraryDependencies += "com.typesafe.akka" %% "akka-stream" % "2.6.19" % Test cross CrossVersion.for3Use2_13

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.13" % Test

enablePlugins(Example)

import meta._
exampleSuperTypes := exampleSuperTypes.value.map {
  case init"_root_.org.scalatest.freespec.AnyFreeSpec" =>
    init"_root_.org.scalatest.freespec.AsyncFreeSpec"
  case otherTrait =>
    otherTrait
}
