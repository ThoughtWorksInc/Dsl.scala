import Ordering.Implicits._

libraryDependencies ++= {
  if (VersionNumber(scalaVersion.value).numbers >= Seq(2L, 13L)) {
    Nil
  } else {
    Seq(
      "com.typesafe.akka" %% "akka-actor" % "2.5.32" % Test,
      "com.typesafe.akka" %% "akka-stream" % "2.5.32" % Test,
      "com.typesafe.akka" %% "akka-http" % "10.1.14" % Test
    )
  }
}

// Skip tests in Scala 2.13 because Akka does not support Scala 2.13 yet
unmanagedSourceDirectories in Test --= {
  if (VersionNumber(scalaVersion.value).numbers >= Seq(2L, 13L)) {
    Seq((scalaSource in Test).value)
  } else {
    Nil
  }
}
