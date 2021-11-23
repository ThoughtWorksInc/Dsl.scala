import Ordering.Implicits._
if (VersionNumber(scalaJSVersion).numbers < Seq(1L)) {
  libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.31"
} else {
  libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.3.5"
}

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")

scalacOptions in Compile ++= {
  scalaBinaryVersion.value match {
    case "2.11" => Seq("–Xexperimental")
    case _      => Seq.empty
  }
}

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.10" % Test
