enablePlugins(Example)

import scala.meta._
exampleSuperTypes := exampleSuperTypes.value.map {
  case ctor"_root_.org.scalatest.FreeSpec" =>
    ctor"_root_.org.scalatest.AsyncFreeSpec"
  case otherTrait =>
    otherTrait
}

libraryDependencies += "com.thoughtworks.dsl" %%% "keywords-each" % "1.2.0+14-7a373cbd" % Optional

libraryDependencies += "com.thoughtworks.dsl" %%% "keywords-fork" % "1.2.0+14-7a373cbd" % Optional
