enablePlugins(Example)

scalacOptions in Compile ++= {
  scalaBinaryVersion.value match {
    case "2.11" => Some("–Xexperimental")
    case _      => None
  }
}

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.6-SNAP2" % Test

libraryDependencies += "com.thoughtworks.dsl" %%% "keywords-each" % "1.2.0" % Test

libraryDependencies += "com.thoughtworks.dsl" %%% "keywords-yield" % "1.2.0" % Test

libraryDependencies += "com.thoughtworks.dsl" %%% "keywords-continue" % "1.2.0" % Test
