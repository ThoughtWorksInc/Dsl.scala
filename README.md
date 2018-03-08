# Dsl.scala
[![Build Status](https://travis-ci.org/ThoughtWorksInc/Dsl.scala.svg?branch=master)](https://travis-ci.org/ThoughtWorksInc/Dsl.scala)
[![Latest version](https://index.scala-lang.org/thoughtworksinc/dsl.scala/dsl/latest.svg)](https://index.scala-lang.org/thoughtworksinc/dsl.scala/dsl)
[![Scaladoc](https://javadoc.io/badge/com.thoughtworks.dsl/dsl_2.12.svg?label=scaladoc)](https://javadoc.io/page/com.thoughtworks.dsl/dsl_2.12/latest/com/thoughtworks/dsl/index.html)

**Dsl.scala** is a framework to create embedded **D**omain-**S**pecific **L**anguages in Scala.

A DSL author is able to create language keywords by implementing the [Dsl](https://javadoc.io/page/com.thoughtworks.dsl/dsl_2.12/latest/com/thoughtworks/dsl/Dsl.html) trait, which contains only one simple function. No knowledge about Scala compiler or AST macros is required.

DSLs written in **Dsl.scala** are collaborative with others DSLs and Scala control flows. A DSL user can create functions that contains interleaved DSLs implemented by different vendors, along with ordinary Scala control flows.

We also provide some built-in keywords, including:
 * The `Shift` keyword for asynchronous programming, similar to `await` / `async` in C#, Python and JavaScript.
 * The `Yield` keyword for generating lazy streams, similar to `yield` in C#, Python and JavaScript.
 * The `Fork` keyword for duplicating current thread, similar to the `fork` system call in POSIX.
 * The `AutoClose` keyword to automatically close resources when exiting a scope, similar to the destructor feature in C++.
 * The `Monadic` keyword for creating monadic control flow, similar to the !-notation in Idris.

All the above keywords can be used together with each others.

## Getting Started

Suppose you want to create a random number generator. The generated numbers should be stored in a lazily evaluated infinite stream, which can be built with the help of our built-in domain-specific keyword `Yield`.

So, you need to add the library that contains the implementation of the keyword `Yield`:

``` scala
// Add the following setting in your build.sbt 
libraryDependencies += "com.thoughtworks.dsl" %% "keywords-yield" % "latest.release"
```

And the Dsl.scala compiler plug-ins that are shared by all DSLs:   

``` scala
// Add the following settings in your build.sbt 
addCompilerPlugin("com.thoughtworks.dsl" %% "compilerplugins-bangnotation" % "latest.release")
addCompilerPlugin("com.thoughtworks.dsl" %% "compilerplugins-reseteverywhere" % "latest.release")
```

The random number generator which can be implemented as a recursive function that produce the next random number in each iteration.

```scala
import com.thoughtworks.dsl.instructions.Yield
def xorshiftRandomGenerator(seed: Int): Stream[Int] = {
  val tmp1 = seed ^ (seed << 13)
  val tmp2 = tmp1 ^ (tmp1 >>> 17)
  val tmp3 = tmp2 ^ (tmp2 << 5)
  !Yield(tmp3)
  xorshiftRandomGenerator(tmp3)
}
```

Note that a keyword is a simply case class. You need a `!` prefix to the keyword to activate the DSL.

It's done. We can test it in ScalaTest:

```scala
val myGenerator = xorshiftRandomGenerator(seed = 123)
myGenerator(0) should be(31682556)
myGenerator(1) should be(-276305998)
myGenerator(2) should be(2101636938)
```

The call to `xorshiftRandomGenerator` does not throw a `StackOverflowError` because the execution of `xorshiftRandomGenerator` will be paused at the keyword `Yield`, and it will be resumed when the caller is looking for the next number.


## Links

* Check the [Scaladoc](https://javadoc.io/page/com.thoughtworks.dsl/dsl_2.12/latest/com/thoughtworks/dsl/index.html) to find examples about using or creating DSLs.
* See [MVNRepository](http://mvnrepository.com/artifact/com.thoughtworks.dsl?sort=newest) or [Scaladex](https://index.scala-lang.org/thoughtworksinc/dsl.scala/dsl) for the settings of each built-in DSLs for your build tools.