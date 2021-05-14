libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.9" % Test

scalacOptions in Compile ++= {
  scalaBinaryVersion.value match {
    case "2.11" => Seq("–Xexperimental")
    case _      => Seq.empty
  }
}

libraryDependencies += "com.thoughtworks.extractor" %%% "extractor" % "2.1.3"
