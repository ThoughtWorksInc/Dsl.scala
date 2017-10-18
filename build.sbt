lazy val CompilerPlugin = project.dependsOn(EachOps % Provided)

lazy val EachOps = project

lazy val `scalaz-bind` = project

lazy val `cats-flatmap` = project

organization in ThisBuild := "com.thoughtworks.each"

scalacOptions in EachOps in Test += raw"""-Xplugin:${(packageBin in CompilerPlugin in Compile).value}"""

crossScalaVersions in ThisBuild := Seq("2.11.11", "2.12.3")
