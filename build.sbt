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
      `keywords-Using` % Test,
      `keywords-Yield` % Test,
      `keywords-Each` % Test
    )

lazy val `keywords-Each` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(Dsl, reset % Test, `keywords-Pure`, `keywords-FlatMap`)

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
    .dependsOn(Dsl, `keywords-TryCatch`)

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
      `keywords-Each` % Test,
      `keywords-Using` % Test,
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
      `keywords-Each` % Test
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
      `keywords-Each` % Test,
      `keywords-Shift` % Test
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
      `keywords-TryCatch`,
      `keywords-TryFinally`,
      `keywords-Shift` % Test,
      `keywords-Yield` % Test
    )

enablePlugins(ScalaUnidocPlugin)

ScalaUnidoc / unidoc / unidocProjectFilter := {
  // Exclude the `keywords-Await`.js due to https://github.com/lampepfl/dotty/issues/14143
  inAnyProject -- inProjects(`keywords-Await`.js)
}

publishArtifact := false
