lazy val CompilerPlugin = project.dependsOn(annotations % Provided)

lazy val annotations = project

lazy val Continuation = project.dependsOn(annotations)

lazy val MayFail = project.dependsOn(Continuation, Await % Test, Yield % Test)

lazy val Await = project.dependsOn(Continuation)

lazy val Yield = project.dependsOn(Continuation, Await % Test)

lazy val `scalaz-bind` = project

lazy val `cats-flatmap` = project

organization in ThisBuild := "com.thoughtworks.each"

scalacOptions in Yield in Test += raw"""-Xplugin:${(packageBin in CompilerPlugin in Compile).value}"""

scalacOptions in Await in Test += raw"""-Xplugin:${(packageBin in CompilerPlugin in Compile).value}"""

scalacOptions in MayFail in Test += raw"""-Xplugin:${(packageBin in CompilerPlugin in Compile).value}"""

crossScalaVersions in ThisBuild := Seq("2.11.11", "2.12.4")
