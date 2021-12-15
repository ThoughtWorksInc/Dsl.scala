// shadow sbt-scalajs' crossProject(JSPlatform, JVMPlatform) and CrossType from Scala.js 0.6.x
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val Dsl =
  crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure).build()

lazy val reset =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(
      Dsl,
      `keywords-Typed`,
      `keywords-Return`,
      `keywords-FlatMap`,
      `keywords-Match`,
      `keywords-Suspend`,
      `keywords-Pure`,
      `keywords-If`,
      `keywords-TryCatch`,
      `keywords-TryFinally`,
      `keywords-TryCatchFinally`,
      `keywords-While`
    )

lazy val `domains-Continuation` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(reset)

lazy val `domains-Task` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(
      `keywords-Shift`,
      reset,
      `domains-Continuation`,
      `keywords-FromIterable` % Test,
      `keywords-Using` % Test,
      `keywords-Yield` % Test,
      `keywords-FromIterable` % Test
    )

lazy val `keywords-FromIterable` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(Dsl, reset, `keywords-Shift`)

lazy val `keywords-Pure` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(Dsl, `keywords-Shift`)

lazy val `keywords-Typed` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(Dsl)

lazy val `keywords-If` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(Dsl)

lazy val `keywords-Match` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(Dsl, `keywords-FlatMap`)

lazy val `keywords-TryCatch` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(Dsl, `keywords-Shift`, `keywords-Match`)

lazy val `keywords-TryCatchFinally` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(Dsl, `keywords-TryCatch`, `keywords-TryFinally`)

lazy val `keywords-TryFinally` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(Dsl, `keywords-Shift`, `keywords-Match`)

lazy val `keywords-While` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(Dsl)

lazy val `keywords-Suspend` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(Dsl)

lazy val `keywords-Return` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(Dsl)

lazy val `keywords-Get` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(Dsl, reset % Test)

lazy val `keywords-Put` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(
      Dsl,
      reset % Test,
      `keywords-Get` % Test,
      `keywords-Yield` % Test,
      `keywords-Return` % Test
    )

lazy val `keywords-AsynchronousIo` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(
      `keywords-Shift`,
      `keywords-FromIterable` % Test,
      `keywords-Using` % Test,
      `keywords-FromIterable` % Test,
      `domains-Task` % Test
    )

lazy val `keywords-Shift` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(Dsl)

lazy val `keywords-Using` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(Dsl, reset, `keywords-Shift`)

lazy val `keywords-FlatMap` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(Dsl, `keywords-Pure`)

lazy val `keywords-NoneSafe` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(Dsl, reset % Test, `keywords-Return`)

lazy val `keywords-Await` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(
      Dsl,
      `domains-Continuation`,
      reset % Test,
      `domains-Task` % Test,
      `keywords-Get` % Test,
      `keywords-Return` % Test,
      `keywords-Yield` % Test,
      `keywords-FromIterable` % Test
    )

lazy val `scala-async` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(`keywords-Await`, reset)

lazy val `keywords-Yield` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(
      Dsl,
      reset % Test,
      `keywords-FromIterable` % Test,
      `keywords-Shift` % Test,
    )

lazy val `keywords-Monadic` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(Dsl, reset % Test)

organization in ThisBuild := "com.thoughtworks.dsl"

skip in publish := true

lazy val `domains-scalaz` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(
      Dsl,
      reset % Test,
      `keywords-Monadic`,
      `keywords-Return`,
      `keywords-Shift` % Test,
      `keywords-Yield` % Test
    )

enablePlugins(ScalaUnidocPlugin)

publishArtifact := false
