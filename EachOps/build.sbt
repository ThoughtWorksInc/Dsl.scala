libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % Test

scalacOptions in Compile ++= {
  if(scalaBinaryVersion.value == "2.11") {
    Seq("–Xexperimental")
  } else {
    Seq()
  }
}