enablePlugins(Example)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7")

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.6-SNAP2" % Test

scalacOptions in Compile ++= {
  scalaBinaryVersion.value match {
    case "2.11" => Seq("–Xexperimental")
    case _      => Seq.empty
  }
}

scalacOptions ++= {
  import Ordering.Implicits._
  if (VersionNumber(scalaVersion.value).numbers >= Seq(2L, 13L)) {
    Seq("-Ymacro-annotations")
  } else {
    Nil
  }
}

libraryDependencies ++= {
  import Ordering.Implicits._
  if (VersionNumber(scalaVersion.value).numbers >= Seq(2L, 13L)) {
    Nil
  } else {
    Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full))
  }
}

libraryDependencies += "com.thoughtworks.enableIf" %% "enableif" % "1.1.6"

libraryDependencies += "com.thoughtworks.dsl" %%% "keywords-yield" % "1.2.0" % Test
