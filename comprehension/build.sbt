enablePlugins(Example)

import meta._
exampleSuperTypes := exampleSuperTypes.value.map {
  case ctor"_root_.org.scalatest.FreeSpec" =>
    ctor"_root_.org.scalatest.AsyncFreeSpec"
  case otherTrait =>
    otherTrait
}

scalacOptions in Compile ++= {
  scalaBinaryVersion.value match {
    case "2.11" => Some("–Xexperimental")
    case _      => None
  }
}

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.6-SNAP2" % Test

libraryDependencies += "com.thoughtworks.dsl" %%% "keywords-each" % "1.3.1" % Test

libraryDependencies += "com.thoughtworks.dsl" %%% "keywords-asynchronousio" % "1.3.1" % Test

libraryDependencies += "com.thoughtworks.dsl" %%% "keywords-yield" % "1.3.1" % Test

libraryDependencies += "com.thoughtworks.dsl" %%% "domains-task" % "1.3.1" % Optional

libraryDependencies += "com.thoughtworks.dsl" %%% "keywords-continue" % "1.3.1" % Test

libraryDependencies += "com.thoughtworks.dsl" %%% "keywords-using" % "1.3.1" % Test
