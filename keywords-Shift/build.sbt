scalacOptions in Compile ++= {
  scalaBinaryVersion.value match {
    case "2.11" => Some("–Xexperimental")
    case _      => None
  }
}
