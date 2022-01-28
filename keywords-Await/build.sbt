import Ordering.Implicits._

enablePlugins(Example)

import scala.meta._
exampleSuperTypes := exampleSuperTypes.value.map {
  case init"_root_.org.scalatest.freespec.AnyFreeSpec" =>
    init"_root_.org.scalatest.freespec.AsyncFreeSpec"
  case otherTrait =>
    otherTrait
}

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.11" % Test
