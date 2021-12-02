libraryDependencies += "org.scalaz" %%% "scalaz-core" % "7.4.0-M9"

libraryDependencies += "org.scalaz" %%% "scalaz-effect" % "7.4.0-M9" % Test

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.10" % Test

enablePlugins(Example)

// import Ordering.Implicits._
// sourceGenerators in Test := {
//   (sourceGenerators in Test).value.filterNot { sourceGenerator =>
//     VersionNumber(scalaVersion.value).numbers >= Seq(2L, 13L) &&
//     sourceGenerator.info
//       .get(taskDefinitionKey)
//       .exists { scopedKey: ScopedKey[_] =>
//         scopedKey.key == generateExample.key
//       }
//   }
// }

import scala.meta._
exampleSuperTypes += init"_root_.org.scalatest.Inside"
