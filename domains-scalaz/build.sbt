libraryDependencies += "org.scalaz" %%% "scalaz-core" % "7.4.0-M9"

libraryDependencies += "com.thoughtworks.tryt" %%% "invariant" % "3.0.0-M0+12-ff2275c1" % Test

libraryDependencies += "org.scalaz" %%% "scalaz-effect" % "7.4.0-M9" % Test

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.10" % Test

enablePlugins(Example)

import scala.meta._
exampleSuperTypes += init"_root_.org.scalatest.Inside"
