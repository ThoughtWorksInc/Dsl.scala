libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.3" % Test

libraryDependencies += "junit" % "junit" % "4.13.2" % Test

libraryDependencies += "com.lihaoyi" %%% "utest" % "0.7.11" % Test

testFrameworks += new TestFramework("utest.runner.Framework")
