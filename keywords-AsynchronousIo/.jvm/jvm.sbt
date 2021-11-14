enablePlugins(Example)

import meta._
exampleSuperTypes := exampleSuperTypes.value.map {
  case init"_root_.org.scalatest.FreeSpec" =>
    init"_root_.org.scalatest.AsyncFreeSpec"
  case otherTrait =>
    otherTrait
}

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.1.4" % Test
