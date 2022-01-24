libraryDependencies += "com.lihaoyi" %%% "utest" % "0.7.11" % Test

testFrameworks += new TestFramework("utest.runner.Framework")
enablePlugins(Example)

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.10" % Test
