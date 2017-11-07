lazy val `delimitedcontinuation-annotations` = project

lazy val `delimitedcontinuation-CompilerPlugin` = project.dependsOn(`delimitedcontinuation-annotations` % Provided)

lazy val Dsl = project.dependsOn(`delimitedcontinuation-annotations`)

lazy val `states-MayFail` = project.dependsOn(Dsl, `instructions-Await` % Test, `instructions-Yield` % Test)

lazy val `instructions-Await` = project.dependsOn(Dsl)

lazy val `instructions-Each` = project.dependsOn(Dsl)

lazy val `instructions-Yield` = project.dependsOn(Dsl, `instructions-Await` % Test)

lazy val `instructions-ScalazBind` = project.dependsOn(Dsl, `instructions-Await` % Test, `instructions-Yield` % Test)

lazy val `instructions-CatsFlatMap` = project.dependsOn(Dsl)

organization in ThisBuild := "com.thoughtworks.dsl"

scalacOptions in `instructions-ScalazBind` in Test += raw"""-Xplugin:${(packageBin in `delimitedcontinuation-CompilerPlugin` in Compile).value}"""

scalacOptions in `instructions-Yield` in Test += raw"""-Xplugin:${(packageBin in `delimitedcontinuation-CompilerPlugin` in Compile).value}"""

scalacOptions in `instructions-Await` in Test += raw"""-Xplugin:${(packageBin in `delimitedcontinuation-CompilerPlugin` in Compile).value}"""

scalacOptions in `states-MayFail` in Test += raw"""-Xplugin:${(packageBin in `delimitedcontinuation-CompilerPlugin` in Compile).value}"""

crossScalaVersions in ThisBuild := Seq("2.11.11", "2.12.4")

lazy val unidoc =
  project
    .enablePlugins(StandaloneUnidoc, TravisUnidocTitle)
    .settings(
      UnidocKeys.unidocProjectFilter in ScalaUnidoc in UnidocKeys.unidoc := inAggregates(LocalRootProject),
      addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3"),
      scalacOptions += "-Xexperimental",
      scalacOptions += "-Ypartial-unification"
    )
