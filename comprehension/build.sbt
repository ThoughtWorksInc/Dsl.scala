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

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.8" % Test
