libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.4" % Test

scalacOptions in Compile ++= {
  scalaBinaryVersion.value match {
    case "2.11" => Some("–Xexperimental")
    case _      => None
  }
}

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

libraryDependencies += "com.thoughtworks.enableIf" %% "enableif" % "1.1.4"
