// shadow sbt-scalajs' crossProject(JSPlatform, JVMPlatform) and CrossType from Scala.js 0.6.x
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val `compilerplugins-BangNotation` = project
  .dependsOn(DslJVM % Test, DslJVM % Provided)
  .settings(
    scalacOptions in Test += raw"""-Xplugin:${(packageBin in Compile).value}""",
    scalacOptions in Test += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
  )

lazy val `compilerplugins-ResetEverywhere` = project.dependsOn(DslJVM % Test, DslJVM % Provided)

lazy val Dsl =
  crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure).build()
lazy val DslJS = Dsl.js
lazy val DslJVM = Dsl.jvm

lazy val `domains-task` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
    )
    .dependsOn(`keywords-Shift`, `keywords-Fork` % Test, `keywords-AutoClose` % Test, `keywords-Yield` % Test)
lazy val `domains-taskJS` = `domains-task`.js
lazy val `domains-taskJVM` = `domains-task`.jvm

lazy val `keywords-Fork` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
    )
    .dependsOn(Dsl, `keywords-Shift`, `keywords-Catch`, `keywords-Hang`, `keywords-Each`)
lazy val `keywords-ForkJS` = `keywords-Fork`.js
lazy val `keywords-ForkJVM` = `keywords-Fork`.jvm

lazy val `keywords-Return` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
    )
    .dependsOn(Dsl)

lazy val `keywords-ReturnJS` = `keywords-Return`.js
lazy val `keywords-ReturnJVM` = `keywords-Return`.jvm

lazy val `keywords-Hang` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
    )
    .dependsOn(Dsl)

lazy val `keywords-HangJS` = `keywords-Hang`.js
lazy val `keywords-HangJVM` = `keywords-Hang`.jvm

lazy val `keywords-AsynchronousIo` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
    )
    .dependsOn(`keywords-Shift`)
lazy val `keywords-AsynchronousIoJS` = `keywords-AsynchronousIo`.js
lazy val `keywords-AsynchronousIoJVM` = `keywords-AsynchronousIo`.jvm

lazy val `keywords-Shift` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
    )
    .dependsOn(Dsl)
lazy val `keywords-ShiftJS` = `keywords-Shift`.js
lazy val `keywords-ShiftJVM` = `keywords-Shift`.jvm

lazy val `keywords-AutoClose` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
    )
    .dependsOn(Dsl, `keywords-Shift`, `keywords-Catch`)
lazy val `keywords-AutoCloseJS` = `keywords-AutoClose`.js
lazy val `keywords-AutoCloseJVM` = `keywords-AutoClose`.jvm

lazy val `keywords-Catch` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
    )
    .dependsOn(Dsl, `keywords-Shift`, `keywords-Yield` % Test)
lazy val `keywords-CatchJS` = `keywords-Catch`.js
lazy val `keywords-CatchJVM` = `keywords-Catch`.jvm

lazy val `keywords-Each` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
    )
    .dependsOn(Dsl, `keywords-Shift`, `keywords-Yield` % Test)
lazy val `keywords-EachJS` = `keywords-Each`.js
lazy val `keywords-EachJVM` = `keywords-Each`.jvm

lazy val `keywords-Await` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
    )
    .dependsOn(Dsl, `keywords-Yield` % Test)
lazy val `keywords-AwaitJS` = `keywords-Await`.js
lazy val `keywords-AwaitJVM` = `keywords-Await`.jvm

lazy val `keywords-Yield` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
    )
    .dependsOn(Dsl, `keywords-Shift` % Test)
lazy val `keywords-YieldJS` = `keywords-Yield`.js
lazy val `keywords-YieldJVM` = `keywords-Yield`.jvm

lazy val `keywords-Monadic` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(Dsl)
lazy val `keywords-MonadicJS` = `keywords-Monadic`.js
lazy val `keywords-MonadicJVM` = `keywords-Monadic`.jvm

lazy val `domains-scalaz` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
    )
    .dependsOn(Dsl, `keywords-Catch`, `keywords-Monadic`, `keywords-Return`, `keywords-Shift` % Test, `keywords-Yield` % Test)
lazy val `domains-scalazJS` = `domains-scalaz`.js
lazy val `domains-scalazJVM` = `domains-scalaz`.jvm

lazy val `domains-cats` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
    )
    .dependsOn(Dsl, `keywords-Catch`, `keywords-Monadic`, `keywords-Return`, `keywords-Shift` % Test, `keywords-Yield` % Test)

lazy val `domains-catsJVM` = `domains-cats`.jvm
lazy val `domains-catsJS` = `domains-cats`.js

lazy val benchmarks = project
  .settings(
    scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
    scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
  )
  .dependsOn(`domains-taskJVM`, `keywords-CatchJVM`, `keywords-ForkJVM`)

lazy val `package` = project
  .settings(
    scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
    scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
  )
  .dependsOn(
    `domains-catsJVM`,
    `domains-scalazJVM`,
    `keywords-ShiftJVM`,
    `keywords-EachJVM`,
    `keywords-YieldJVM`,
    `keywords-ForkJVM`,
    `keywords-AwaitJVM`,
    `keywords-AsynchronousIoJVM`,
    `keywords-AutoCloseJVM`,
    `domains-taskJVM`,
    DslJVM
  )

organization in ThisBuild := "com.thoughtworks.dsl"

crossScalaVersions in ThisBuild := Seq("2.11.12", "2.12.6")

scalacOptions in ThisBuild ++= {
  if (scalaBinaryVersion.value == "2.11") {
    Some("-Ybackend:GenBCode")
  } else {
    None
  }
}

lazy val unidoc =
  project
    .enablePlugins(StandaloneUnidoc, TravisUnidocTitle)
    .settings(
      unidocProjectFilter in ScalaUnidoc in BaseUnidocPlugin.autoImport.unidoc := {
        inDependencies(`package`) ||
        inDependencies(`compilerplugins-BangNotation`) ||
        inDependencies(`compilerplugins-ResetEverywhere`)
      },
      addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4"),
      scalacOptions += "-Xexperimental",
      scalacOptions += "-Ypartial-unification"
    )

publishArtifact := false

parallelExecution in Global := false
