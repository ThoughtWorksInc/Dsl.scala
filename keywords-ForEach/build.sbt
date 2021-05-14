enablePlugins(Example)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.9" % Test

scalacOptions in Compile ++= {
  scalaBinaryVersion.value match {
    case "2.11" => Seq("–Xexperimental")
    case _      => Seq.empty
  }
}
