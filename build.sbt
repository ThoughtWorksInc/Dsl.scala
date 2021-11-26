// shadow sbt-scalajs' crossProject(JSPlatform, JVMPlatform) and CrossType from Scala.js 0.6.x
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val Dsl =
  crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure).build()

lazy val bangnotation =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(
      Dsl,
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

lazy val `domains-task` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(
      `keywords-Shift`,
      bangnotation % Test,
      `keywords-In` % Test,
      `keywords-Fork` % Test,
      `keywords-Using` % Test,
      `keywords-Yield` % Test,
      comprehension % Test
    )

lazy val `keywords-Fork` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(
      Dsl,
      bangnotation % Test,
      `keywords-Shift`,
      `keywords-Catch` % Optional,
      `keywords-Continue`,
      `keywords-In`
    )

lazy val `keywords-Pure` =
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

lazy val `keywords-Continue` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(Dsl, bangnotation % Test, `keywords-Each` % Test)

lazy val `keywords-Get` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(Dsl, bangnotation % Test)

lazy val `keywords-Put` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(
      Dsl,
      bangnotation % Test,
      `keywords-Get` % Test,
      `keywords-Yield` % Test,
      `keywords-Return` % Test
    )

lazy val `keywords-AsynchronousIo` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(
      `keywords-Shift`,
      `keywords-In` % Test,
      `keywords-Using` % Test,
      comprehension % Test,
      `domains-task` % Test
    )

lazy val `keywords-Shift` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(Dsl)

lazy val `keywords-Catch` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(Dsl, bangnotation % Test, `keywords-Shift`, `keywords-Yield` % Test)

lazy val `keywords-Using` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(Dsl, bangnotation % Test, `keywords-Shift`, `keywords-Catch` % Optional)

lazy val `keywords-Map` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(Dsl, bangnotation % Test)

lazy val `keywords-FlatMap` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(Dsl, `keywords-Pure`)

lazy val `keywords-WithFilter` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(`keywords-Continue`)

lazy val `keywords-NoneSafe` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(Dsl, bangnotation % Test, `keywords-Return`)

lazy val `keywords-NullSafe` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(Dsl, bangnotation % Test)

lazy val `keywords-For` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(Dsl, bangnotation % Test, `keywords-In` % Test)

lazy val `keywords-In` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(Dsl, bangnotation % Test, `keywords-Shift`)

lazy val `keywords-Await` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(
      Dsl,
      comprehension % Test,
      bangnotation % Test,
      `domains-task` % Test,
      `keywords-Catch` % Test,
      `keywords-In` % Test,
      `keywords-Get` % Test,
      `keywords-Return` % Test,
      `keywords-Yield` % Test
    )

lazy val `keywords-Yield` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(
      Dsl,
      bangnotation % Test,
      `keywords-Shift` % Test,
      `keywords-In` % Test,
      `keywords-Continue` % Test
    )

lazy val `keywords-Monadic` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(Dsl, bangnotation % Test)

lazy val comprehension =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(
      `keywords-Map`,
      `keywords-FlatMap`,
      `keywords-WithFilter`,
      `keywords-Return`,
      `keywords-Yield` % Test,
      `keywords-In` % Test,
      `keywords-Using` % Test,
      `keywords-Continue` % Test,
      bangnotation % Test
    )

organization in ThisBuild := "com.thoughtworks.dsl"

skip in publish := true

lazy val `keywords-ForEach` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(Dsl, bangnotation % Test, `keywords-Each` % Test)

lazy val `keywords-Each` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(Dsl, bangnotation % Test, `keywords-Shift`)

lazy val `domains-scalaz` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(
      Dsl,
      bangnotation % Test,
      `keywords-Catch` % Optional,
      `keywords-Monadic`,
      `keywords-Return`,
      `keywords-Shift` % Test,
      `keywords-Yield` % Test
    )

lazy val `package` = project
  .dependsOn(
    `domains-scalaz`.jvm,
    `keywords-Get`.jvm,
    `keywords-Put`.jvm,
    `keywords-Continue`.jvm,
    `keywords-Return`.jvm,
    `keywords-Shift`.jvm,
    `keywords-ForEach`.jvm,
    `keywords-Each`.jvm,
    `keywords-Yield`.jvm,
    `keywords-Fork`.jvm,
    `keywords-NoneSafe`.jvm,
    `keywords-NullSafe`.jvm,
    `keywords-Await`.jvm,
    `keywords-AsynchronousIo`.jvm,
    `keywords-Using`.jvm,
    `keywords-Map`.jvm,
    `keywords-FlatMap`.jvm,
    `keywords-WithFilter`.jvm,
    `comprehension`.jvm,
    `bangnotation`.jvm,
    `domains-task`.jvm,
    Dsl.jvm
  )

lazy val unidoc =
  project
    .enablePlugins(ScalaUnidocPlugin)
    .settings(
      publishArtifact := false,
      unidocProjectFilter in ScalaUnidoc in BaseUnidocPlugin.autoImport.unidoc := {
        inDependencies(`package`)
      }
    )
