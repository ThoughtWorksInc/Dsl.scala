enablePlugins(Example)

import scala.meta._
exampleSuperTypes += ctor"_root_.org.scalatest.Inside"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.20"

libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % "7.2.20" % Test

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

scalacOptions in Compile ++= {
  scalaBinaryVersion.value match {
    case "2.11" => Seq("–Xexperimental")
    case _      => Seq.empty
  }
}

scalacOptions in Test += "-Ypartial-unification"

libraryDependencies += "com.thoughtworks.tryt" %% "invariant" % "2.0.3" % Test
