enablePlugins(Example)
libraryDependencies -= "org.scalatest" %%% "scalatest" % "3.0.5" % Test

import scala.meta._
exampleSuperTypes += ctor"_root_.org.scalatest.Inside"

libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % "7.2.25" % Test

libraryDependencies += "com.thoughtworks.tryt" %% "invariant" % "2.0.3" % Test
