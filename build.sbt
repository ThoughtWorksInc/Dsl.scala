lazy val `compilerplugins-BangNotation` = project.dependsOn(Dsl % Test, Dsl % Provided)

lazy val `compilerplugins-ResetEverywhere` = project.dependsOn(Dsl % Test, Dsl % Provided)

lazy val Dsl = project

lazy val `domains-Raii` =
  project.dependsOn(`instructions-Hang`,
                    `instructions-Scope`,
                    `instructions-Catch`,
                    `instructions-Shift`,
                    `instructions-Fork` % Test,
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
                    `domains-Raii` % Test)

lazy val `package` = project.dependsOn(
  `instructions-Shift`,
  `instructions-CatsFlatMap`,
  `instructions-Each`,
  `instructions-ScalazBind`,
  `instructions-Yield`,
  `domains-Raii`,
  `compilerplugins-BangNotation`,
  `compilerplugins-ResetEverywhere`,
  Dsl
)

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
  `domains-Raii`,
  `instructions-Arm`,
  `instructions-AutoClose`
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
        inAggregates(LocalRootProject)
      },
      addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3"),
      scalacOptions += "-Xexperimental",
      scalacOptions += "-Ypartial-unification"
    )

publishArtifact := false
