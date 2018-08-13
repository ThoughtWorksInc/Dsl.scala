enablePlugins(Example)
libraryDependencies -= "org.scalatest" %%% "scalatest" % "3.0.5" % Test

sourceGenerators in Test := {
  (sourceGenerators in Test).value.filterNot { sourceGenerator =>
    import Ordering.Implicits._
    VersionNumber(scalaVersion.value).numbers >= Seq(2L, 13L) &&
    sourceGenerator.info
      .get(taskDefinitionKey)
      .exists { scopedKey: ScopedKey[_] =>
        scopedKey.key == generateExample.key
      }
  }
}

import scala.meta._
exampleSuperTypes += ctor"_root_.org.scalatest.Inside"

libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % "7.2.25" % Test

libraryDependencies += "com.thoughtworks.tryt" %% "invariant" % "2.0.4" % Test
