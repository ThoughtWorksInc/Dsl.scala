# Dsl.scala <a href="http://thoughtworks.com/"><img align="right" src="https://www.thoughtworks.com/imgs/tw-logo.png" title="ThoughtWorks" height="15"/></a>

[![Build Status](https://travis-ci.org/ThoughtWorksInc/Dsl.scala.svg?branch=master)](https://travis-ci.org/ThoughtWorksInc/Dsl.scala)
[![Latest version](https://index.scala-lang.org/thoughtworksinc/dsl.scala/dsl/latest.svg)](https://index.scala-lang.org/thoughtworksinc/dsl.scala)
[![Scaladoc](https://javadoc.io/badge/com.thoughtworks.dsl/dsl_2.12.svg?label=scaladoc)](https://javadoc.io/page/com.thoughtworks.dsl/dsl_2.12/latest/com/thoughtworks/dsl/index.html)

**Dsl.scala** is a framework to create embedded **D**omain-**S**pecific **L**anguages in Scala.

A DSL author is able to create language keywords by implementing the [Dsl](https://javadoc.io/page/com.thoughtworks.dsl/dsl_2.12/latest/com/thoughtworks/dsl/Dsl.html) trait, which contains only one abstract method to be implemented. No knowledge about Scala compiler or AST macros is required.

DSLs written in **Dsl.scala** are collaborative with others DSLs and Scala control flows. A DSL user can create functions that contains interleaved DSLs implemented by different vendors, along with ordinary Scala control flows.

We also provide some built-in keywords, including:

 * The `Await` keyword for creating memoized asynchronous values as Scala [Future](https://docs.scala-lang.org/overviews/core/futures.html)s, similar to the `await` / `async` keywords in C#, Python and JavaScript.
 * The `Shift` keyword for creating asynchronous tasks as delimited continuations, similar to the `shift` operator in [Scala Continuations](https://github.com/scala/scala-continuations).
 * The `AsynchronousIo.Connect`, `AsynchronousIo.Accept`, `AsynchronousIo.Read` and `AsynchronousIo.Write` keyword for perform I/O on an asynchronous channel.
 * The `Yield` keyword for generating lazy streams, similar to `yield` in C#, Python and JavaScript.
 * The `Each` keyword for iterating on a collection, similar to the list comprehension feature in Scala, Haskell, OCaml, Python and Lisp.
 * The `Hang` keyword for terminating the current context, similar to the `scala.sys.exit()`.
 * The `Fork` keyword for duplicating current context, similar to the `fork` system call in POSIX.
 * The `Return` keyword for early returning, similar to the native `return` keyword in Scala, except `Return` works in `Future` or other domains.
 * The `AutoClose` keyword to automatically close resources when exiting a scope, similar to the destructor feature in C++.
 * The `Monadic` keyword for creating Scalaz or Cats monadic control flow, similar to the !-notation in Idris.

All the above keywords can be used together with each others. For example you can perform list comprehension to manipulate native resources in an asynchronous task by using `Each`, `AutoClose` and `Shift` together.

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

The random number generator can be implemented as a recursive function that produces the next random number in each iteration.

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

Note that a keyword is a plain case class. You need a `!` prefix to the keyword to activate the DSL.

It's done. We can test it in ScalaTest:

```scala
val myGenerator = xorshiftRandomGenerator(seed = 123)
myGenerator(0) should be(31682556)
myGenerator(1) should be(-276305998)
myGenerator(2) should be(2101636938)
```

The call to `xorshiftRandomGenerator` does not throw a `StackOverflowError` because the execution of `xorshiftRandomGenerator` will be paused at the keyword `Yield`, and it will be resumed when the caller is looking for the next number.

## Showcases

* [sbt-ammonite-classpath](https://github.com/ThoughtWorksInc/sbt-ammonite-classpath) is an sbt plug-in that [uses `Each` keywords to iterate through configuations and keys](https://github.com/ThoughtWorksInc/sbt-ammonite-classpath/blob/793bc20/src/main/scala/com/thoughtworks/deeplearning/sbtammoniteclasspath/AmmoniteClasspath.scala#L23), as an alternative syntax of `for` comprehensions.
  
(Feel free to add your project here)

## Links

* Check the [Documentation](https://javadoc.io/page/com.thoughtworks.dsl/dsl_2.12/latest/com/thoughtworks/dsl/index.html) to find examples about using or creating DSLs.
* See [MVNRepository](http://mvnrepository.com/artifact/com.thoughtworks.dsl?sort=newest) or [Scaladex](https://index.scala-lang.org/thoughtworksinc/dsl.scala) for the settings of each built-in DSLs for your build tools.
* [Benchmarks: Dsl.scala vs Monix vs Cats Effect vs Scalaz Concurrent vs Scala Async vs Scala Continuation](https://github.com/ThoughtWorksInc/Dsl.scala/wiki/Benchmarks:-Dsl.scala-vs-Monix-vs-Cats-Effect-vs-Scalaz-Concurrent-vs-Scala-Async-vs-Scala-Continuation)
