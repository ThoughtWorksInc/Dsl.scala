enablePlugins(Example)
libraryDependencies -= "org.scalatest" %%% "scalatest" % "3.0.5" % Test

import Ordering.Implicits._

libraryDependencies ++= {
  if (VersionNumber(scalaVersion.value).numbers >= Seq(2L, 13L)) {
    None
  } else {
    Some("org.typelevel" %%% "cats-effect" % "0.9" % Test)
  }
}

sourceGenerators in Test := {
  (sourceGenerators in Test).value.filterNot { sourceGenerator =>
    VersionNumber(scalaVersion.value).numbers >= Seq(2L, 13L) &&
    sourceGenerator.info
      .get(taskDefinitionKey)
      .exists { scopedKey: ScopedKey[_] =>
        scopedKey.key == generateExample.key
      }
  }
}
