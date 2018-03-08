lazy val `compilerplugins-BangNotation` = project.dependsOn(Dsl % Test, Dsl % Provided)

lazy val `compilerplugins-ResetEverywhere` = project.dependsOn(Dsl % Test, Dsl % Provided)

lazy val Dsl = project

lazy val `domains-Raii` =
  project.dependsOn(
    `instructions-Hang`,
    `instructions-Scope`,
    `instructions-Catch`,
    `instructions-Shift`,
    `instructions-AutoClose`,
    `instructions-Fork` % Test,
    `instructions-Yield` % Test
  )

lazy val `instructions-Fork` =
  project.dependsOn(Dsl,
                    `instructions-Scope`,
                    `instructions-Shift`,
                    `instructions-Catch`,
                    `instructions-Hang`,
                    `instructions-Each`)

lazy val `instructions-Hang` = project.dependsOn(Dsl)

lazy val `instructions-Shift` = project.dependsOn(Dsl)

lazy val `instructions-AutoClose` = project.dependsOn(Dsl)

lazy val `instructions-Catch` = project.dependsOn(Dsl, `instructions-Shift`, `instructions-Yield` % Test)

lazy val `instructions-Scope` = project.dependsOn(Dsl, `instructions-Shift`, `instructions-Yield` % Test)

lazy val `instructions-Each` = project.dependsOn(Dsl, `instructions-Shift` % Test, `instructions-Yield` % Test)

lazy val `instructions-Yield` = project.dependsOn(Dsl, `instructions-Shift` % Test)

lazy val `instructions-Monadic` = project.dependsOn(Dsl)

lazy val `domains-scalaz` =
  project.dependsOn(Dsl,
                    `instructions-Scope`,
                    `instructions-Catch`,
                    `instructions-Monadic`,
                    `instructions-Shift` % Test,
                    `instructions-Yield` % Test)

lazy val `domains-cats` =
  project.dependsOn(Dsl,
                    `instructions-Scope`,
                    `instructions-Catch`,
                    `instructions-Monadic`,
                    `instructions-Shift` % Test,
                    `instructions-Yield` % Test)

lazy val `benchmarks-TaskBenchmark` = project.dependsOn(`domains-Raii`, `instructions-Yield`)

lazy val `package` = project.dependsOn(
  `instructions-Shift`,
  `domains-cats`,
  `instructions-Each`,
  `domains-scalaz`,
  `instructions-Yield`,
  `instructions-Fork`,
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
  `domains-cats`,
  `instructions-Each`,
  `domains-scalaz`,
  `instructions-Yield`,
  LocalProject("package"),
  `domains-Raii`,
  `instructions-AutoClose`,
  `benchmarks-TaskBenchmark`
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
      addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4"),
      scalacOptions += "-Xexperimental",
      scalacOptions += "-Ypartial-unification"
    )

publishArtifact := false
