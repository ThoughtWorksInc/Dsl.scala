import Ordering.Implicits._

libraryDependencies ++= {
  if (VersionNumber(scalaVersion.value).numbers >= Seq(2L, 13L)) {
    Nil
  } else {
    Seq(
      "com.typesafe.akka" %% "akka-actor" % "2.5.14" % Test,
      "com.typesafe.akka" %% "akka-testkit" % "2.5.14" % Test,
      "com.typesafe.akka" %% "akka-http" % "10.1.3" % Test,
      "com.typesafe.akka" %% "akka-http-testkit" % "10.1.3" % Test
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
