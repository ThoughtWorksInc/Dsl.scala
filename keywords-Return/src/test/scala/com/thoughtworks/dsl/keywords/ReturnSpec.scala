package com.thoughtworks.dsl.keywords
import org.scalatest.{FreeSpec, Matchers}
import com.thoughtworks.dsl.Dsl.!!

/**
  * @author 杨博 (Yang Bo)
  */
final class ReturnSpec extends FreeSpec with Matchers {

  "return a Stream" in {
    def stream: Stream[Int] = !Return[Int](1)
    stream should be(Stream(1))
  }

  "return the left domain" in {
    def continuation: Int !! String = !Return(42)

    continuation { s =>
      throw new AssertionError(s)
    } should be(42)
  }

  "return the right domain" in {
    def continuation: Int !! String = !Return("right value")

    continuation { s =>
      s should be("right value")
      43
    } should be(43)
  }

  "return the middle domain" - {

    "as the return value" in {
      def continuation: Int !! Double !! String = !Return(1.23)

      continuation { s =>
        throw new AssertionError(s)
      } { d =>
        d should be(1.23)
        43
      } should be(43)
    }

    "then the throw expression will not be executed" in {
      def continuation: Int !! Double !! String = {
        throw !Return(1.23)
      }

      continuation { s =>
        throw new AssertionError(s)
      } { d =>
        d should be(1.23)
        43
      } should be(43)
    }
  }

}
