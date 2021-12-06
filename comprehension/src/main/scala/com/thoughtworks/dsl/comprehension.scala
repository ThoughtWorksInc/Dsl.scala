package com.thoughtworks.dsl

import scala.language.higherKinds
import scala.language.implicitConversions
import com.thoughtworks.dsl.keywords._

private[dsl] sealed trait LowPriorityComprehension0 {
  import com.thoughtworks.dsl.comprehension._

  implicit def comprehensionOps[From, Keyword, Value](from: From)(implicit
      asKeyword: From => Dsl.Keyword[Keyword, Value] with Keyword
  ): ComprehensionOps[Keyword, Value] =
    new ComprehensionOps[Keyword, Value](from)
}

/** Provides utilities to perform `for`-comprehension on [[Dsl.Keyword]].
  *
  * Add the following import statement to enable `for`-comprehension on keywords:
  *
  * {{{
  * import com.thoughtworks.dsl.comprehension._
  * }}}
  *
  * @example `for` / `yield` expressions can be used on keywords.
  *
  *          {{{
  *          import com.thoughtworks.dsl.keywords._
  *
  *          def cartesianProduct = for {
  *            i <- Each(Array(1, 2, 3))
  *            j <- Each(Vector(1, 10, 100, 1000))
  *          } yield i * j
  *          }}}
  *
  *          The results of `for` / `yield` expressions are also keywords.
  *
  *          {{{
  *          cartesianProduct should be(a[Dsl.Keyword[_, _]])
  *          }}}
  *
  *          You can use !-notation extract the value from the produced keyword.
  *
  *          {{{
  *          def resultAsList = List(!cartesianProduct)
  *          resultAsList should be(List(1, 10, 100, 1000, 2, 20, 200, 2000, 3, 30, 300, 3000))
  *
  *          def resultAsSet: Set[Int] = !Return(!cartesianProduct)
  *          resultAsSet should be(Set(1, 10, 100, 1000, 2, 20, 200, 2000, 3, 30, 300, 3000))
  *          }}}
  *
  *          Alternatively, [[comprehension.ComprehensionOps.to]] can be used to convert the result of a keyword to other types of values as well.
  *
  *          {{{
  *          cartesianProduct.to[List] should be(List(1, 10, 100, 1000, 2, 20, 200, 2000, 3, 30, 300, 3000))
  *          }}}
  * @example This example implements the same feature as the example on Scaladoc of [[keywords.Yield]],
  *          except this example use `for`-comprehension instead of !-notation.
  *
  *          {{{
  *          import com.thoughtworks.dsl.Dsl
  *          import com.thoughtworks.dsl.keywords._
  *          import com.thoughtworks.dsl.comprehension._
  *
  *          def gccFlagBuilder(sourceFile: String, includes: String*) = {
  *            for {
  *              _ <- Yield("gcc")
  *              _ <- Yield("-c")
  *              _ <- Yield(sourceFile)
  *              include <- Each(includes)
  *              _ <- Yield("-I")
  *              _ <- Yield(include)
  *              r <- Continue
  *            } yield r: String
  *          }
  *
  *          gccFlagBuilder("main.c", "lib1/include", "lib2/include").to[Stream] should be(Stream("gcc", "-c", "main.c", "-I", "lib1/include", "-I", "lib2/include"))
  *          }}}
  *
  *          Alternatively, you can use Scala native `yield` keyword to produce the last value.
  *
  *          {{{
  *          def gccFlagBuilder2(sourceFile: String, includes: String*) = {
  *            for {
  *              _ <- Yield("gcc")
  *              _ <- Yield("-c")
  *              _ <- Yield(sourceFile)
  *              include <- Each(includes)
  *              _ <- Yield("-I")
  *            } yield include
  *          }
  *          gccFlagBuilder2("main.c", "lib1/include", "lib2/include").to[Stream] should be(Stream("gcc", "-c", "main.c", "-I", "lib1/include", "-I", "lib2/include"))
  *          }}}
  *
  *          You can also omit the explicit constructor of [[keywords.Yield]] with the help of implicit conversion [[keywords.Yield.implicitYield]].
  *
  *          {{{
  *          import com.thoughtworks.dsl.keywords.Yield.implicitYield
  *
  *          def augmentString = ()
  *          def wrapString = ()
  *          }}}
  *
  *          Note that [[scala.Predef.augmentString]] and [[scala.Predef.wrapString]] must be disabled in order to use `flatMap` for [[keywords.Yield]].
  *
  *          {{{
  *          def gccFlagBuilder3(sourceFile: String, includes: String*) = {
  *            for {
  *              _ <- "gcc"
  *              _ <- "-c"
  *              _ <- sourceFile
  *              include <- Each(includes)
  *              _ <- "-I"
  *            } yield include
  *          }
  *          gccFlagBuilder3("main.c", "lib1/include", "lib2/include").to[Stream] should be(Stream("gcc", "-c", "main.c", "-I", "lib1/include", "-I", "lib2/include"))
  *          }}}
  */
object comprehension extends LowPriorityComprehension0 {

  @inline
  private def resetDomain[Keyword, Domain](keyword: Keyword)(implicit dsl: Dsl[Keyword, Domain, Domain]): Domain = {
    dsl.cpsApply(keyword, implicitly)
  }

  final class ComprehensionOps[Keyword, Value] private[dsl] (private val keyword: Keyword) extends AnyVal {

    import keywords._

    def map[MapResult](mapper: Value => MapResult): Map[Keyword, Value, MapResult] = Map(keyword, mapper)

    def flatMap[MapResult, NestedKeyword, NestedValue](mapper: Value => MapResult)(implicit
        asKeywordMapper: (Value => MapResult) => Value => Dsl.Keyword[NestedKeyword, NestedValue] with NestedKeyword
    ): FlatMap[Keyword, Value, NestedKeyword, NestedValue] = FlatMap(keyword, mapper)

    def withFilter(condition: Value => Boolean): WithFilter[Keyword, Value] = WithFilter(keyword, condition)

    def to[Output[_]](implicit
        dsl: Dsl[Keyword, Output[Value], Value],
        returnDsl: Dsl[Return[Value], Output[Value], Nothing]
    ): Output[Value] = {
      dsl.cpsApply(
        keyword,
        { value: Value =>
          resetDomain(Return(value))
        }
      )
    }
    def as[Domain](implicit
        dsl: Dsl[Keyword, Domain, Value],
        returnDsl: Dsl[Return[Value], Domain, Nothing]
    ): Domain = {
      dsl.cpsApply(
        keyword,
        { value: Value =>
          resetDomain(Return(value))
        }
      )
    }

  }

  implicit def nothingComprehensionOps[From, Keyword](from: From)(implicit
      asKeyword: From => Dsl.Keyword[Keyword, Nothing] with Keyword
  ): ComprehensionOps[Keyword, Nothing] =
    new ComprehensionOps[Keyword, Nothing](from)

}
