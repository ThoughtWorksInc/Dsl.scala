addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.4" % Test

scalacOptions in Compile ++= {
  scalaBinaryVersion.value match {
    case "2.11" => Seq("–Xexperimental")
    case _      => Seq.empty
  }
}

scalacOptions in Test += "-Ypartial-unification"
