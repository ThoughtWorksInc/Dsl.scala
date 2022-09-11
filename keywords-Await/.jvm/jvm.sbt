import Ordering.Implicits._

libraryDependencies += "com.typesafe.akka" %% "akka-http" % "10.2.9" % Test cross CrossVersion.for3Use2_13

libraryDependencies += "com.typesafe.akka" %% "akka-actor-typed" % "2.6.20" % Test cross CrossVersion.for3Use2_13

libraryDependencies += "com.typesafe.akka" %% "akka-stream" % "2.6.20" % Test cross CrossVersion.for3Use2_13
