enablePlugins(Example)

import scala.meta._
exampleSuperTypes := exampleSuperTypes.value.map {
  case ctor"_root_.org.scalatest.freespec.AnyFreeSpec" =>
    ctor"_root_.org.scalatest.freespec.AsyncFreeSpec"
  case otherTrait =>
    otherTrait
}

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.9" % Test
