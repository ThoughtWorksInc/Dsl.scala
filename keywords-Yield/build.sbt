libraryDependencies += "com.lihaoyi" %%% "utest" % "0.8.1" % Test

testFrameworks += new TestFramework("utest.runner.Framework")

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.14" % Test

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
    Seq(
      compilerPlugin(
        "org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full
      )
    )
  }
}
