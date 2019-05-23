enablePlugins(Example)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7")

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.6-SNAP2" % Test

scalacOptions in Compile ++= {
  scalaBinaryVersion.value match {
    case "2.11" => Seq("–Xexperimental")
    case _      => Seq.empty
  }
}

libraryDependencies += "com.thoughtworks.dsl" %%% "keywords-each" % "1.3.0" % Test
