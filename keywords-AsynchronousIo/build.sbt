scalacOptions ++= {
  import Ordering.Implicits._
  if (VersionNumber(scalaVersion.value).numbers < Seq(2L, 12L)) {
    // Enable SAM types for Scala 2.11
    Some("-Xexperimental")
  } else {
    None
  }
}

libraryDependencies += "com.thoughtworks.dsl" %%% "comprehension" % "1.2.0" % Optional

libraryDependencies += "com.thoughtworks.dsl" %%% "domains-task" % "1.2.0" % Optional

libraryDependencies += "com.thoughtworks.dsl" %%% "keywords-using" % "1.2.0" % Optional

libraryDependencies += "com.thoughtworks.dsl" %%% "keywords-each" % "1.2.0+22-af129ac9" % Optional
