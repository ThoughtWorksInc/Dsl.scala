lazy val `delimitedcontinuation-annotations` = project

lazy val `delimitedcontinuation-CompilerPlugin` =
  project.dependsOn(`delimitedcontinuation-annotations` % Test, `delimitedcontinuation-annotations` % Provided)

lazy val Dsl = project.dependsOn(`delimitedcontinuation-annotations`)

lazy val `domains-ExceptionHandling` = project.dependsOn(Dsl, `instructions-Shift` % Test, `instructions-Yield` % Test)

lazy val `instructions-Shift` = project.dependsOn(Dsl)

lazy val `instructions-Each` = project.dependsOn(Dsl, `instructions-Shift` % Test, `instructions-Yield` % Test)

lazy val `instructions-Yield` = project.dependsOn(Dsl, `instructions-Shift` % Test)

lazy val `instructions-ScalazBind` = project.dependsOn(Dsl, `instructions-Shift` % Test, `instructions-Yield` % Test)

lazy val `instructions-CatsFlatMap` = project.dependsOn(Dsl, `instructions-Shift` % Test, `instructions-Yield` % Test)

organization in ThisBuild := "com.thoughtworks.dsl"

for {
  testingProject <- Seq(
    `instructions-Shift`,
    `instructions-CatsFlatMap`,
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
      unidocProjectFilter in ScalaUnidoc in BaseUnidocPlugin.autoImport.unidoc := inAggregates(LocalRootProject),
      addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3"),
      scalacOptions += "-Xexperimental",
      scalacOptions += "-Ypartial-unification"
    )

publishArtifact := false
