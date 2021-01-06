// shadow sbt-scalajs' crossProject(JSPlatform, JVMPlatform) and CrossType from Scala.js 0.6.x
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val `compilerplugins-BangNotation` = project
  .dependsOn(Dsl.jvm % Test, Dsl.jvm % Provided)
  .settings(
    scalacOptions in Test += raw"""-Xplugin:${(packageBin in Compile).value}""",
    scalacOptions in Test += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
  )

lazy val `compilerplugins-ResetEverywhere` = project.dependsOn(Dsl.jvm % Test, Dsl.jvm % Provided)

lazy val Dsl =
  crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure).build()

lazy val `domains-task` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
    )
    .dependsOn(
      `keywords-Shift`,
      `keywords-Each` % Test,
      `keywords-Fork` % Test,
      `keywords-Using` % Test,
      `keywords-Yield` % Test,
      `comprehension` % Test
    )

lazy val `keywords-Fork` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
    )
    .dependsOn(Dsl, `keywords-Shift`, `keywords-Catch` % Optional, `keywords-Continue`, `keywords-ForEach`)

lazy val `keywords-Return` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
    )
    .dependsOn(Dsl)

lazy val `keywords-Continue` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
    )
    .dependsOn(Dsl, `keywords-Each` % Test)

lazy val `keywords-Get` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
    )
    .dependsOn(Dsl)

lazy val `keywords-Put` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
    )
    .dependsOn(Dsl, `keywords-Get` % Test, `keywords-Yield` % Test, `keywords-Return` % Test)

lazy val `keywords-AsynchronousIo` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
    )
    .dependsOn(
      `keywords-Shift`,
      `keywords-Each` % Test,
      `keywords-Using` % Test,
      `comprehension` % Test,
      `domains-task` % Test
    )

lazy val `keywords-Shift` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
    )
    .dependsOn(Dsl)

lazy val `keywords-Catch` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
    )
    .dependsOn(Dsl, `keywords-Shift`, `keywords-Yield` % Test)

lazy val `keywords-Using` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
    )
    .dependsOn(Dsl, `keywords-Shift`, `keywords-Catch` % Optional)

lazy val `keywords-Map` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
    )
    .dependsOn(Dsl)

lazy val `keywords-FlatMap` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
    )
    .dependsOn(Dsl)

lazy val `keywords-WithFilter` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
    )
    .dependsOn(`keywords-Continue`)

lazy val `keywords-NoneSafe` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
    )
    .dependsOn(Dsl, `keywords-Return`)

lazy val `keywords-NullSafe` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
    )
    .dependsOn(Dsl)

lazy val `keywords-ForEach` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
    )
    .dependsOn(Dsl, `keywords-Each` % Test)

lazy val `keywords-Each` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
    )
    .dependsOn(Dsl, `keywords-Shift`)

lazy val `keywords-Await` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
    )
    .dependsOn(
      Dsl,
      `domains-task` % Test,
      `keywords-Catch` % Test,
      `keywords-Get` % Test,
      `keywords-Return` % Test,
      `keywords-Yield` % Test
    )

lazy val `keywords-Yield` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
    )
    .dependsOn(Dsl, `keywords-Shift` % Test, `keywords-Each` % Test, `keywords-Continue` % Test)

lazy val `keywords-Monadic` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(Dsl)

lazy val `domains-scalaz` =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
    )
    .dependsOn(
      Dsl,
      `keywords-Catch` % Optional,
      `keywords-Monadic`,
      `keywords-Return`,
      `keywords-Shift` % Test,
      `keywords-Yield` % Test
    )

lazy val comprehension =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
      scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
    )
    .dependsOn(
      `keywords-Map`,
      `keywords-FlatMap`,
      `keywords-WithFilter`,
      `keywords-Return`,
      `keywords-Each` % Test,
      `keywords-Yield` % Test,
      `keywords-Using` % Test,
      `keywords-Continue` % Test
    )

lazy val `package` = project
  .settings(
    scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
    scalacOptions += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
  )
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
    `domains-task`.jvm,
    Dsl.jvm
  )

// Replace `keywords-Catch` % Optional to `keywords-Catch` for Scala 2.11
for (catchProject <- `keywords-Catch`.projects.values.toSeq) yield {
  Global / buildDependencies := {
    val oldBuildDependencies = (Global / buildDependencies).value
    val catchProjectRef = (catchProject / thisProjectRef).value
    if (scalaBinaryVersion.value == "2.11") {
      internal.BuildDependencies(
        oldBuildDependencies.classpath.mapValues(_.map {
          case ResolvedClasspathDependency(`catchProjectRef`, Some(Optional.name)) =>
            ResolvedClasspathDependency(catchProjectRef, None)
          case dep =>
            dep
        }),
        oldBuildDependencies.aggregate
      )
    } else {
      oldBuildDependencies
    }
  }
}

organization in ThisBuild := "com.thoughtworks.dsl"

scalacOptions in ThisBuild ++= {
  if (scalaBinaryVersion.value == "2.11") {
    Some("-Ybackend:GenBCode")
  } else {
    None
  }
}

lazy val unidoc =
  project
    .enablePlugins(ScalaUnidocPlugin)
    .settings(
      publishArtifact := false,
      unidocProjectFilter in ScalaUnidoc in BaseUnidocPlugin.autoImport.unidoc := {
        import Ordering.Implicits._
        if (VersionNumber(scalaVersion.value).numbers >= Seq(2L, 13L)) {
          // Workaround for https://github.com/scala/bug/issues/11045
          (
            inDependencies(`package`) ||
              inDependencies(`compilerplugins-BangNotation`) ||
              inDependencies(`compilerplugins-ResetEverywhere`)
          ) --
            inProjects(
              Dsl.jvm,
              `keywords-Continue`.jvm,
              `keywords-Yield`.jvm,
              `domains-task`.jvm,
              `keywords-Each`.jvm,
              `keywords-Fork`.jvm
            )
        } else {
          inDependencies(`package`) ||
          inDependencies(`compilerplugins-BangNotation`) ||
          inDependencies(`compilerplugins-ResetEverywhere`)
        }
      },
      addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3"),
      scalacOptions += "-Xexperimental",
      scalacOptions ++= {
        import Ordering.Implicits._
        if (VersionNumber(scalaVersion.value).numbers >= Seq(2L, 13L)) {
          Seq("-Ymacro-annotations")
        } else {
          Nil
        }
      },
      libraryDependencies ++= {
        import Ordering.Implicits._
        if (VersionNumber(scalaVersion.value).numbers >= Seq(2L, 13L)) {
          Nil
        } else {
          Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full))
        }
      }
    )

skip in publish := true

parallelExecution in Global := {
  import Ordering.Implicits._
  VersionNumber(scalaVersion.value).numbers >= Seq(2L, 12L)
}
