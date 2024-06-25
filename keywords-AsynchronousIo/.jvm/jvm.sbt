enablePlugins(Example)

import meta._
exampleSuperTypes := {
  val (init"_root_.org.scalatest.freespec.AnyFreeSpec" +: traits) =
    exampleSuperTypes.value
  init"_root_.org.scalatest.freespec.AsyncFreeSpec" +: traits
}

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.19" % Test

libraryDependencies += "com.lihaoyi" %%% "sourcecode" % "0.3.0" % Test
