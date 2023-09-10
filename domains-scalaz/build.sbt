libraryDependencies += "org.scalaz" %%% "scalaz-core" % "7.3.7"

libraryDependencies += "com.thoughtworks.tryt" %%% "invariant" % "3.0.0" % Test

libraryDependencies += "org.scalaz" %%% "scalaz-effect" % "7.3.7" % Test

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.17" % Test

enablePlugins(Example)

import scala.meta._
exampleSuperTypes += init"_root_.org.scalatest.Inside"
