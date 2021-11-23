// shadow sbt-scalajs' crossProject(JSPlatform, JVMPlatform) and CrossType from Scala.js 0.6.x
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val Dsl = project

lazy val bangnotation =
  project.dependsOn(
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
  project.dependsOn(
    `keywords-Shift`,
    bangnotation % Test,
    `keywords-ForYield` % Test,
    `keywords-Fork` % Test,
    `keywords-Using` % Test,
    `keywords-Yield` % Test,
    comprehension % Test
  )

lazy val `keywords-Fork` =
  project.dependsOn(
    Dsl,
    bangnotation % Test,
    `keywords-Shift`,
    `keywords-Catch` % Optional,
    `keywords-Continue`,
    `keywords-ForYield`
  )

lazy val `keywords-Pure` =
  project.dependsOn(Dsl)

lazy val `keywords-If` =
  project.dependsOn(Dsl)

lazy val `keywords-Match` =
  project.dependsOn(Dsl, `keywords-FlatMap`)

lazy val `keywords-TryCatch` =
  project.dependsOn(Dsl, `keywords-Shift`, `keywords-Match`)

lazy val `keywords-TryCatchFinally` =
  project.dependsOn(Dsl, `keywords-TryCatch`, `keywords-TryFinally`)

lazy val `keywords-TryFinally` =
  project.dependsOn(Dsl, `keywords-Shift`, `keywords-Match`)

lazy val `keywords-While` =
  project.dependsOn(Dsl)

lazy val `keywords-Suspend` =
  project.dependsOn(Dsl)

lazy val `keywords-Return` =
  project.dependsOn(Dsl)

lazy val `keywords-Continue` =
  project.dependsOn(Dsl, bangnotation % Test, `keywords-ForYield` % Test)

lazy val `keywords-Get` =
  project.dependsOn(Dsl, bangnotation % Test)

lazy val `keywords-Put` =
  project.dependsOn(
    Dsl,
    bangnotation % Test,
    `keywords-Get` % Test,
    `keywords-Yield` % Test,
    `keywords-Return` % Test
  )

lazy val `keywords-AsynchronousIo` =
  project.dependsOn(
    `keywords-Shift`,
    `keywords-ForYield` % Test,
    `keywords-Using` % Test,
    comprehension % Test,
    `domains-task` % Test
  )

lazy val `keywords-Shift` =
  project.dependsOn(Dsl)

lazy val `keywords-Catch` =
  project.dependsOn(Dsl, bangnotation % Test, `keywords-Shift`, `keywords-Yield` % Test)

lazy val `keywords-Using` =
  project.dependsOn(Dsl, bangnotation % Test, `keywords-Shift`, `keywords-Catch` % Optional)

lazy val `keywords-Map` =
  project.dependsOn(Dsl, bangnotation % Test)

lazy val `keywords-FlatMap` =
  project.dependsOn(Dsl, `keywords-Pure`)

lazy val `keywords-WithFilter` =
  project.dependsOn(`keywords-Continue`)

lazy val `keywords-NoneSafe` =
  project.dependsOn(Dsl, bangnotation % Test, `keywords-Return`)

lazy val `keywords-NullSafe` =
  project.dependsOn(Dsl, bangnotation % Test)

lazy val `keywords-For` =
  project.dependsOn(Dsl, bangnotation % Test, `keywords-In` % Test)

lazy val `keywords-ForYield` =
  project.dependsOn(Dsl, bangnotation % Test, `keywords-In` % Test)

lazy val `keywords-In` =
  project.dependsOn(Dsl, bangnotation % Test, `keywords-Shift`)

lazy val `keywords-Await` =
  project.dependsOn(
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
  project.dependsOn(
    Dsl,
    bangnotation % Test,
    `keywords-Shift` % Test,
    `keywords-ForYield` % Test,
    `keywords-Continue` % Test
  )

lazy val `keywords-Monadic` =
  project.dependsOn(Dsl, bangnotation % Test)

lazy val `scalaz` =
  project.dependsOn(
    Dsl,
    bangnotation % Test,
    `keywords-Catch` % Optional,
    `keywords-Monadic`,
    `keywords-Return`,
    `keywords-Shift` % Test,
    `keywords-Yield` % Test
  )

lazy val comprehension =
  project.dependsOn(
    `keywords-Map`,
    `keywords-FlatMap`,
    `keywords-WithFilter`,
    `keywords-Return`,
    `keywords-ForYield` % Test,
    `keywords-Yield` % Test,
    `keywords-In` % Test,
    `keywords-Using` % Test,
    `keywords-Continue` % Test,
    bangnotation % Test,
  )

organization in ThisBuild := "com.thoughtworks.dsl"

skip in publish := true
