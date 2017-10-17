libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % Provided

autoCompilerPlugins := true

scalacOptions in Compile ++= {
  if(scalaBinaryVersion.value == "2.11") {
    Seq("–Xexperimental")
  } else {
    Seq()
  }
}