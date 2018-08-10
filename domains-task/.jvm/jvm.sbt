enablePlugins(Example)
libraryDependencies -= "org.scalatest" %%% "scalatest" % "3.0.5" % Test

import scala.meta._
exampleSuperTypes := exampleSuperTypes.value.map {
  case ctor"_root_.org.scalatest.FreeSpec" =>
    ctor"_root_.org.scalatest.AsyncFreeSpec"
  case otherTrait =>
    otherTrait
}
