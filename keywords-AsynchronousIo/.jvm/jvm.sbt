enablePlugins(Example)

import meta._
exampleSuperTypes := {
  val (init"_root_.org.scalatest.freespec.AnyFreeSpec" +: traits) = exampleSuperTypes.value
  init"_root_.org.scalatest.freespec.AsyncFreeSpec" +: traits
}

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.10" % Test
