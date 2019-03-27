libraryDependencies += "org.typelevel" %%% "cats-free" % "1.4.0" % Test

libraryDependencies += "org.typelevel" %%% "cats-free" % "1.4.0" % Optional // For Scaladoc

libraryDependencies += "org.typelevel" %%% "cats-core" % "1.4.0"

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.6-SNAP2" % Test

scalacOptions ++= {
  import Ordering.Implicits._
  if (VersionNumber(scalaVersion.value).numbers < Seq(2L, 12L)) {
    // Enable SAM types for Scala 2.11
    Some("-Xexperimental")
  } else {
    None
  }
}
