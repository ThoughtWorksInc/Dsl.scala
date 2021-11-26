libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test

libraryDependencies += "junit" % "junit" % "4.12" % Test

enablePlugins(Example)

import scala.meta._
exampleSuperTypes := exampleSuperTypes.value.map {
  case init"_root_.org.scalatest.freespec.AnyFreeSpec" =>
    init"_root_.org.scalatest.freespec.AsyncFreeSpec"
  case otherTrait =>
    otherTrait
}

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.10" % Test
