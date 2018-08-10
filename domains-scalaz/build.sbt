libraryDependencies += "org.scalaz" %%% "scalaz-core" % "7.2.25"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7")

scalacOptions in Compile ++= {
  scalaBinaryVersion.value match {
    case "2.11" => Seq("–Xexperimental")
    case _      => Seq.empty
  }
}

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.6-SNAP1" % Test
