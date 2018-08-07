package com.thoughtworks.dsl.keywords
import org.scalatest.{FreeSpec, Matchers}
import Return._
import com.thoughtworks.dsl.Dsl.!!

/**
  * @author 杨博 (Yang Bo)
  */
final class ReturnSpec extends FreeSpec with Matchers {

  "return a Stream" in {
    def s = !Return[Int, Stream[Int]](1)

    s should be(Stream(1))
  }

  "return the left domain" in {
    def continuation: Int !! String = !Return[Int, Int !! String](42)

    continuation { s =>
      throw new AssertionError(s)
    } should be(42)
  }

  "return the right domain" in {
    def continuation: Int !! String = !Return[String, Int !! String]("right value")

    continuation { s =>
      s should be("right value")
      43
    } should be(43)
  }

  "return the middle domain" - {

    "as the return value" in {
      def continuation: Int !! Double !! String = !Return[Double, Int !! Double !! String](1.23)

      continuation { s =>
        throw new AssertionError(s)
      } { d =>
        d should be(1.23)
        43
      } should be(43)
    }

    "then the throw expression will not be executed" in {
      def continuation = {
        throw !Return[Double, Exception](1.23)
      }: Int !! Double !! String

      continuation { s =>
        throw new AssertionError(s)
      } { d =>
        d should be(1.23)
        43
      } should be(43)
    }
  }

}
