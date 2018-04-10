lazy val `compilerplugins-BangNotation` = project.dependsOn(Dsl % Test, Dsl % Provided)

lazy val `compilerplugins-ResetEverywhere` = project.dependsOn(Dsl % Test, Dsl % Provided)

lazy val Dsl = project

lazy val task =
  project.dependsOn(`keywords-Shift`, `keywords-Fork` % Test, `keywords-AutoClose` % Test, `keywords-Yield` % Test)

lazy val `domains-Raii` =
  project.dependsOn(
    `keywords-Hang`,
    `keywords-Scope`,
    `keywords-Catch`,
    `keywords-Shift`,
    `keywords-AutoClose`,
    `keywords-Fork` % Test,
    `keywords-Yield` % Test
  )

lazy val `keywords-Fork` =
  project.dependsOn(Dsl, `keywords-Scope`, `keywords-Shift`, `keywords-Catch`, `keywords-Hang`, `keywords-Each`)

lazy val `keywords-Hang` = project.dependsOn(Dsl)

lazy val `keywords-AsynchronousIo` = project.dependsOn(`keywords-Shift`)

lazy val `keywords-Shift` = project.dependsOn(Dsl)

lazy val `keywords-AutoClose` = project.dependsOn(Dsl, `keywords-Shift`, `keywords-Catch`, `keywords-Scope`)

lazy val `keywords-Catch` = project.dependsOn(Dsl, `keywords-Shift`, `keywords-Yield` % Test)

lazy val `keywords-Scope` = project.dependsOn(Dsl, `keywords-Shift`, `keywords-Yield` % Test)

lazy val `keywords-Each` = project.dependsOn(Dsl, `keywords-Shift` % Test, `keywords-Yield` % Test)

lazy val `keywords-Yield` = project.dependsOn(Dsl, `keywords-Shift` % Test)

lazy val `keywords-Monadic` = project.dependsOn(Dsl)

lazy val `domains-scalaz` =
  project.dependsOn(Dsl,
                    `keywords-Scope`,
                    `keywords-Catch`,
                    `keywords-Monadic`,
                    `keywords-Shift` % Test,
                    `keywords-Yield` % Test)

lazy val `domains-cats` =
  project.dependsOn(Dsl,
                    `keywords-Scope`,
                    `keywords-Catch`,
                    `keywords-Monadic`,
                    `keywords-Shift` % Test,
                    `keywords-Yield` % Test)

lazy val `benchmarks-TaskBenchmark` = project.dependsOn(task, `keywords-Catch`, `keywords-Scope`)

lazy val `package` = project.dependsOn(
  `compilerplugins-BangNotation`,
  `compilerplugins-ResetEverywhere`,
  `domains-cats`,
  `domains-scalaz`,
  `domains-Raii`,
  `keywords-Shift`,
  `keywords-Each`,
  `keywords-Yield`,
  `keywords-Fork`,
  `keywords-AsynchronousIo`,
  LocalProject("task"),
  Dsl
)

organization in ThisBuild := "com.thoughtworks.dsl"

Seq[ProjectReference](
  `domains-cats`,
  `domains-scalaz`,
  `domains-Raii`,
  `keywords-Fork`,
  `keywords-Catch`,
  `keywords-Hang`,
  `keywords-Scope`,
  `keywords-Shift`,
  `keywords-Each`,
  `keywords-AsynchronousIo`,
  `keywords-Yield`,
  `keywords-AutoClose`,
  `benchmarks-TaskBenchmark`,
  LocalProject("task"),
  LocalProject("package")
).flatMap { testingProject =>
  Seq(
    scalacOptions in testingProject += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
    scalacOptions in testingProject += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
  )
}

crossScalaVersions in ThisBuild := Seq("2.11.12", "2.12.5")

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
