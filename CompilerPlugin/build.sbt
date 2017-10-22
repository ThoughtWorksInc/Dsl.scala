libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % Provided

autoCompilerPlugins := true

scalacOptions ++= {
  if (scalaBinaryVersion.value == "2.11") {
    Seq("–Yexperimental")
  } else {
    Seq()
  }
}
