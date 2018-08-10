libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.6-SNAP1" % Test

scalacOptions in Compile ++= {
  scalaBinaryVersion.value match {
    case "2.11" => Seq("–Xexperimental")
    case _      => Seq.empty
  }
}
