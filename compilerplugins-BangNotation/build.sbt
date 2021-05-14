libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % Provided

scalacOptions ++= {
  if (scalaBinaryVersion.value == "2.11") {
    Seq("–Yexperimental")
  } else {
    Seq()
  }
}

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.9" % Test
