enablePlugins(JmhPlugin)

optimization := true

publishArtifact := false

libraryDependencies += "org.scala-lang.modules" %% "scala-async" % "0.9.7"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.5.10"

libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % "7.2.25"

libraryDependencies += "com.twitter" %% "algebird-core" % "0.13.4"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.5"

libraryDependencies += "com.thoughtworks.binding" %% "binding" % "11.0.1"

libraryDependencies += "com.thoughtworks.each" %% "each" % "3.3.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0"

libraryDependencies += "io.monix" %% "monix" % "2.3.3"

libraryDependencies += "io.monix" %% "monix-scalaz-72" % "2.3.3"

libraryDependencies += "org.typelevel" %% "cats-effect" % "0.10"

libraryDependencies += {
  VersionNumber(scalaVersion.value).numbers match {
    case Seq(2L, 12L, minor) if minor > 2L =>
      compilerPlugin("org.scala-lang.plugins" % "scala-continuations-plugin_2.12.2" % "latest.release")
    case _ =>
      compilerPlugin("org.scala-lang.plugins" % "scala-continuations-plugin" % "latest.release" cross CrossVersion.patch)
  }
}

libraryDependencies += "org.scala-lang.plugins" %% "scala-continuations-library" % "latest.release"

scalacOptions += "-P:continuations:enable"

libraryDependencies += "org.scala-lang.modules" %% "scala-async" % "0.9.7"