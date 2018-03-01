lazy val `compilerplugins-BangNotation` = project.dependsOn(Dsl % Test, Dsl % Provided)

lazy val `compilerplugins-ResetEverywhere` = project.dependsOn(Dsl % Test, Dsl % Provided)

lazy val Dsl = project

lazy val task =
  project.dependsOn(`instructions-Fork`, `domains-ExceptionHandling`, `instructions-Arm`)

lazy val `domains-ExceptionHandling` =
  project.dependsOn(`instructions-Hang`,
                    `instructions-Scope`,
                    `instructions-Catch`,
                    `instructions-Shift` % Test,
                    `instructions-Yield` % Test)

lazy val `instructions-Fork` =
  project.dependsOn(Dsl,
                    `instructions-Scope`,
                    `instructions-Shift`,
                    `instructions-Catch`,
                    `instructions-Hang`,
                    `instructions-Each`)

lazy val `instructions-Hang` = project.dependsOn(Dsl)

lazy val `instructions-Shift` = project.dependsOn(Dsl)

lazy val `instructions-AutoClose` =
  project.dependsOn(`instructions-Catch`, `instructions-Scope`, `instructions-Shift`)

lazy val `instructions-Catch` = project.dependsOn(Dsl, `instructions-Shift`, `instructions-Yield` % Test)

lazy val `instructions-Scope` = project.dependsOn(Dsl, `instructions-Shift`, `instructions-Yield` % Test)

lazy val `instructions-Each` = project.dependsOn(Dsl, `instructions-Shift` % Test, `instructions-Yield` % Test)

lazy val `instructions-Yield` = project.dependsOn(Dsl, `instructions-Shift` % Test)

lazy val `instructions-ScalazBind` =
  project.dependsOn(Dsl,
                    `instructions-Scope`,
                    `instructions-Catch`,
                    `instructions-Shift` % Test,
                    `instructions-Yield` % Test)

lazy val `instructions-CatsFlatMap` =
  project.dependsOn(Dsl,
                    `instructions-Scope`,
                    `instructions-Catch`,
                    `instructions-Shift` % Test,
                    `instructions-Yield` % Test)

lazy val `instructions-Arm` =
  project.dependsOn(`instructions-Catch`,
                    `instructions-Scope`,
                    `instructions-Shift`,
                    `instructions-Yield` % Test,
                    `domains-ExceptionHandling` % Test)

organization in ThisBuild := "com.thoughtworks.dsl"

Seq[ProjectReference](
  `instructions-Fork`,
  `instructions-Catch`,
  `instructions-Hang`,
  `instructions-Scope`,
  `instructions-Shift`,
  `instructions-CatsFlatMap`,
  `instructions-Each`,
  `instructions-ScalazBind`,
  `instructions-Yield`,
  `domains-ExceptionHandling`,
  `instructions-Arm`,
  `instructions-AutoClose`,
  LocalProject("task")
).flatMap { testingProject =>
  Seq(
    scalacOptions in testingProject += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
    scalacOptions in testingProject += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
  )
}

crossScalaVersions in ThisBuild := Seq("2.11.12", "2.12.4")

lazy val unidoc =
  project
    .enablePlugins(StandaloneUnidoc, TravisUnidocTitle)
    .settings(
      unidocProjectFilter in ScalaUnidoc in BaseUnidocPlugin.autoImport.unidoc := {
        inAggregates(LocalRootProject) -- inProjects(`compilerplugins-BangNotation`)
      },
      addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3"),
      scalacOptions += "-Xexperimental",
      scalacOptions += "-Ypartial-unification"
    )

publishArtifact := false
