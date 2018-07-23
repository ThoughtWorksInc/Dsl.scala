enablePlugins(Example)

import scala.meta._
exampleSuperTypes += ctor"_root_.org.scalatest.Inside"

libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % "7.2.20" % Test

libraryDependencies += "com.thoughtworks.tryt" %% "invariant" % "2.0.3" % Test
