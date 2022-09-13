libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.3" % Test

libraryDependencies += "junit" % "junit" % "4.13.2" % Test

libraryDependencies += "com.lihaoyi" %%% "utest" % "0.8.0" % Test

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.13" % Test

testFrameworks += new TestFramework("utest.runner.Framework")
