libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test

libraryDependencies += "junit" % "junit" % "4.12" % Test

libraryDependencies += "com.lihaoyi" %%% "utest" % "0.7.10" % Test

testFrameworks += new TestFramework("utest.runner.Framework")
