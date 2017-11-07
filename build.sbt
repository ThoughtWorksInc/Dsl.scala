lazy val `delimitedcontinuation-annotations` = project

lazy val `delimitedcontinuation-CompilerPlugin` =
  project.dependsOn(`delimitedcontinuation-annotations` % Test, `delimitedcontinuation-annotations` % Provided)

lazy val Dsl = project.dependsOn(`delimitedcontinuation-annotations`)

lazy val `domains-ExceptionHandling` = project.dependsOn(Dsl, `instructions-Await` % Test, `instructions-Yield` % Test)

lazy val `instructions-Await` = project.dependsOn(Dsl)

lazy val `instructions-Each` = project.dependsOn(Dsl, `instructions-Await` % Test, `instructions-Yield` % Test)

lazy val `instructions-Yield` = project.dependsOn(Dsl, `instructions-Await` % Test)

lazy val `instructions-ScalazBind` = project.dependsOn(Dsl, `instructions-Await` % Test, `instructions-Yield` % Test)

lazy val `instructions-CatsFlatMap` = project.dependsOn(Dsl)

organization in ThisBuild := "com.thoughtworks.dsl"

for {
  testingProject <- Seq(
    `instructions-Await`,
    `instructions-Each`,
    `instructions-ScalazBind`,
    `instructions-Yield`,
    `domains-ExceptionHandling`,
    `delimitedcontinuation-CompilerPlugin`,
    `delimitedcontinuation-annotations`
  )
} yield {
  scalacOptions in testingProject in Test += raw"""-Xplugin:${(packageBin in `delimitedcontinuation-CompilerPlugin` in Compile).value}"""
}

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
