package com.thoughtworks.dsl

import bangnotation._
import Dsl.Run
import Dsl.!!
import Dsl.IsKeyword
import Dsl.Typed
import keywords._, Match._
import org.junit._, Assert._
import concurrent.ExecutionContext.Implicits.global
import concurrent.Future
import concurrent.Await.result
import concurrent.duration._
import scala.language.dynamics
import org.hamcrest.CoreMatchers.instanceOf

class AwaitTest {

  @Test
  def testReturnIf: Unit = { 
    val reified = reify {
      if (true) {
        !Return(!Await(Future(42)))
      }
      -1
    }
    assertEquals(42, result(reified.to[Future], Duration.Inf))
  }

  type Id[A] = A

  @Test
  def testComprehension1: Unit = {
    def inner1 = for {
      j <- In(0 until 3)
    } yield 100 + j
    summon[Run[
      FlatMap[
        In[Int],
        Int,
        Pure[Int]
      ],
      Vector[Int],
      Int
    ]]

    summon[
      IsKeyword[
        FlatMap[
          In[Int],
          Int,
          Pure[Int]
        ],
        Int
      ]
    ]

    val ast1 = Await(Future(1)).flatMap { i =>
      inner1
    }
    summon[
      ast1.type
        <:< 
      FlatMap[
        Await[Int],
        Int,
        FlatMap[
          In[Int],
          Int,
          Pure[Int]
        ]
      ]
    ]

    def forall[A] = {
      summon[Run[
        FlatMap[
          Await[Int],
          Int,
          FlatMap[
            In[Int],
            Int,
            Pure[Int]
          ]
        ],
        Future[A] !! Vector[Int],
        Int
      ]]
    }

    import FlatMap.given

    // assertEquals(Vector(100, 101, 102), result(ast1.as[Future[Vector[Int]]], Duration.Inf))
    assertEquals(Vector(100, 101, 102), result(ast1.as[Future[Vector[Int]] !! Vector[Int]].apply(Future.successful), Duration.Inf))
    assertArrayEquals(Array(100, 101, 102), result(ast1.as[Future[Array[Int]] !! Array[Int]].apply(Future.successful), Duration.Inf))
    // assertArrayEquals(Array(100, 101, 102), result(ast1.as[Future[Future[Array[Int]] !! Array[Int]]].flatMap(_(Future.successful)), Duration.Inf))
  }

  @Test
  def testComprehension2: Unit = {
    val inner2 = for {
      j <- In(0 until 10)
    } yield 111
    summon[
      inner2.type
        <:<
      FlatMap[
        In[Int],
        Int,
        Pure[Int]
      ]
    ]   
    val ast2 = Await(Future(1)).flatMap { i =>
      inner2
    }
    summon[
      ast2.type
        <:< 
      FlatMap[
        Await[Int],
        Int,
        FlatMap[
          In[Int],
          Int,
          Pure[Int]
        ]
      ]
    ]
  }

  @Test
  def testComprehension3: Unit = {
    val ast3 = for {
      i <- Await(Future(1))
      j <- In(0 until 10)
    } yield 111
    summon[
      ast3.type
        <:< 
      FlatMap[
        Await[Int],
        Int,
        FlatMap[
          In[Int],
          Int,
          Pure[Int]
        ]
      ]
    ]
  }

  @Test
  def test1: Unit = {
    val refied = reify[1](1)
    summon[refied.type <:< Typed[Pure[1], 1]]
    assertEquals(1, refied)
  }

  @Test
  def test2: Unit = {
    val refied = reify[1] {1}
    summon[refied.type <:< Typed[Pure[1], 1]]
    assertEquals(1, refied)
    
    val refied2 = reify[1](!refied)
    // summon[refied2.type <:< Typed[Typed[Pure[1], 1], 1]]
    summon[refied2.type <:< Typed[refied.type, 1]]
    assertEquals(1, refied2)

  }
  
  @Test
  def test3: Unit = {
    val refied = reify {
      
    }
    summon[refied.type <:< Typed[Pure[Unit], Unit]]
    assertEquals((), refied)
  }

  @Test
  def test4: Unit = {
    val refied = reify {
      !Await(Future(1L))
    }
    summon[refied.type <:< Typed[Await[Long], Long]]
    summon[Run[Typed[Await[Long], Long], Future[Long], Long]](refied)
    assertEquals(1L, result(refied.to[Future], Duration.Inf))
  }

  @Test
  def test5: Unit = {
    val refied = reify {
      !Await(Future(!Await(Future(1L))))
    }
    summon[refied.type <:< Typed[FlatMap[Await[Long], Long, Await[Long]], Long]]
    summon[
      Run[
        FlatMap[Await[Long], Long, Await[Long]],
        Future[Long],
        Long
      ]
    ]
    assertEquals(1L, result(refied.to[Future], Duration.Inf))
  }

  @Test
  def test6: Unit = {
    val refied = reify {
      val i = !Await(Future(super.getClass.getSimpleName))
      !Await(Future(i))
    }
    summon[refied.type <:< Typed[FlatMap[Await[String], String, Await[String]], String]]
    summon[
      Run[
        FlatMap[Await[String], String, Await[String]],
        Future[String],
        String
      ]
    ]
    assertEquals(super.getClass.getSimpleName, result(refied.to[Future], Duration.Inf))
  }
  object Foo extends Dynamic {
    def applyDynamicNamed(method: String)(arg1: (String, Long)) = {
      arg1._2
    }
  }

  @Test
  def test7: Unit = {
    val refied = reify {
      val i = !Await(Future(body=this.Foo.d(j=1L)))
      object A {
        def j = i + 200
      }
      !Await(Future(A.j))
    }
    summon[refied.type <:< Typed[FlatMap[Await[Long], Long, Await[Long]], Long]]
    summon[
      Run[
        FlatMap[Await[Long], Long, Await[Long]],
        Future[Long],
        Long
      ]
    ]
    assertEquals(201L, result(refied.to[Future], Duration.Inf))
  }


  @Test
  def testReturn1: Unit = {
    val refied = reify {
      (return !Await(Future(()))): Unit
    }
    // summon[refied.type <:<
    //   com.thoughtworks.dsl.Dsl.Typed[
    //     com.thoughtworks.dsl.keywords.FlatMap[
    //       com.thoughtworks.dsl.keywords.Await[Unit]
    //     , Unit, 
    //       com.thoughtworks.dsl.keywords.FlatMap[
    //         com.thoughtworks.dsl.keywords.Return[Unit]
    //       , Nothing, com.thoughtworks.dsl.keywords.Pure[Unit]]
    //     ]
    //   , Unit]
    // ]
    assertEquals((), result(refied.to[Future], Duration.Inf))
  }

  @Test
  def testReturn2: Unit = {
    val refied = reify[Nothing] {
      return !Await(Future(()))
    }
    // summon[refied.type <:<
    //   com.thoughtworks.dsl.Dsl.Typed[
    //    FlatMap[
    //      Await[Unit]
    //     , Unit,Pure[Nothing]]
    //   , Nothing]
    // ]
    assertEquals((), result(refied.as[Future[Unit]], Duration.Inf))
  }

  inline val x = 42

  @Test
  def testInline: Unit = {
    val rr = reify {
      reify[x.type] {
        x
      }
    }

    summon[rr.type <:<
      com.thoughtworks.dsl.Dsl.Typed[
       Pure[
          com.thoughtworks.dsl.Dsl.Typed[
           Pure[42]
          , 42]
        ]
      , 
        com.thoughtworks.dsl.Dsl.Typed[
         Pure[42]
        , 42]
      ]
    ]
    
    assertEquals(42, rr.to[Id].to[Id])

  }

  @Test
  def testIf: Unit = {
    val generator = reify {
      if (false) {
        !Yield(0)
      }
      if (true) {
        !Yield(1)
      }
      if ({ !Yield(2); false }) {
        !Yield(3)
      } else {
        !Yield(4)
      }
      LazyList.empty[Int]
    }

    assertEquals(Seq(1, 2, 4), generator.to[Id])
  }

  @Test
  def testMatch: Unit = {
    def loop(i: Int): LazyList[Int] = {
      val reified = reify {
        (i: Int) match {
          case 100 =>
            LazyList.empty
          case _ =>
            !Yield(i)
            loop(i + 1)
        }
      }
      // summon[reified.type <:<
      //   com.thoughtworks.dsl.Dsl.Typed[
      //     com.thoughtworks.dsl.keywords.FlatMap[
      //       com.thoughtworks.dsl.Dsl.Typed[
      //         com.thoughtworks.dsl.keywords.Pure[?]
      //       , Int]
      //     , Int, 
      //       com.thoughtworks.dsl.keywords.Match.WithIndex[(0), 
      //         com.thoughtworks.dsl.keywords.Pure[LazyList[Nothing]]
      //       ]
      //     +: 
      //       com.thoughtworks.dsl.keywords.Match.WithIndex[(1), 
      //         com.thoughtworks.dsl.keywords.FlatMap[
      //           com.thoughtworks.dsl.keywords.Yield[Int]
      //         , Unit, com.thoughtworks.dsl.keywords.Pure[LazyList[Int]]]
      //       ]
      //     +: Nothing]
      //   , LazyList[Int]]
      // ]
      reified.as[LazyList[Int]]
    }

    assertEquals(LazyList(90, 91, 92, 93, 94, 95, 96, 97, 98, 99), loop(90))
  }

  @Test
  def testTryCatchFinally: Unit = {
    val reified = reify {
      try {
        0 / 0
        !Await(Future("unreachable code"))
      } catch {
        case _: ArithmeticException =>
          s"Cannot divide ${!Await(Future(3))} by ${!Await(Future(0))}"
      } finally {}
    }

    summon[reified.type <:<
      com.thoughtworks.dsl.Dsl.Typed[
        com.thoughtworks.dsl.keywords.TryCatchFinally[
          com.thoughtworks.dsl.keywords.Suspend[
            com.thoughtworks.dsl.keywords.Await[String]
          ]
        , 
          com.thoughtworks.dsl.keywords.Match.WithIndex[0, 
            com.thoughtworks.dsl.keywords.FlatMap[
              com.thoughtworks.dsl.keywords.Await[Int]
            , Int, 
              com.thoughtworks.dsl.keywords.FlatMap[
                com.thoughtworks.dsl.keywords.Await[Int]
              , Int, com.thoughtworks.dsl.keywords.Pure[String]]
            ]
          ]
        +: Nothing, com.thoughtworks.dsl.keywords.Pure[Unit]]
      , String]
    ]

    assertEquals("Cannot divide 3 by 0", result(reified.as[Future[String]], Duration.Inf))
  }

  @Test(expected = classOf[ArithmeticException])
  def testTryFinally: Unit = {
    val reified = reify {
      try {
        0 / 0
        !Await(Future(-1))
      } finally {}
    }

    summon[
      reified.type <:< 
        com.thoughtworks.dsl.Dsl.Typed[
          com.thoughtworks.dsl.keywords.TryFinally[
            com.thoughtworks.dsl.keywords.Suspend[
              com.thoughtworks.dsl.keywords.Await[Int]
            ]
          , com.thoughtworks.dsl.keywords.Pure[Unit]]
        , Int]
    ]

    result(reified.as[Future[Int]], Duration.Inf)
  }

  @Test
  def testTryCatch: Unit = {
    val refied = reify {
      try {
        // s"Cannot divide ${!Await(Future(3))} by ${!Await(Future(0))}"
        // s"Division result: ${math.random()}"
        // s"Division result: ${!Await(Future(3)) / !Await(Future(0))}"
        // raw"Division result: ${!Await(Future(3)) / !Await(Future(0))}"
        "Division result: " + (!Await(Future(3)) / !Await(Future(0)))
      } catch {
        case e: ArithmeticException =>
          s"Cannot divide ${!Await(Future(3))} by ${!Await(Future(0))}"
      }
    }

    summon[refied.type <:<
      com.thoughtworks.dsl.Dsl.Typed[
        com.thoughtworks.dsl.keywords.TryCatch[
          com.thoughtworks.dsl.keywords.Suspend[
            com.thoughtworks.dsl.keywords.FlatMap[
              com.thoughtworks.dsl.keywords.Await[Int]
            , Int, 
              com.thoughtworks.dsl.keywords.FlatMap[
                com.thoughtworks.dsl.keywords.Await[Int]
              , Int, com.thoughtworks.dsl.keywords.Pure[String]]
            ]
          ]
        , 
          com.thoughtworks.dsl.keywords.Match.WithIndex[0, 
            com.thoughtworks.dsl.keywords.FlatMap[
              com.thoughtworks.dsl.keywords.Await[Int]
            , Int, 
              com.thoughtworks.dsl.keywords.FlatMap[
                com.thoughtworks.dsl.keywords.Await[Int]
              , Int, com.thoughtworks.dsl.keywords.Pure[String]]
            ]
          ]
        +: Nothing]
      , String]
    ]

    assertEquals("Cannot divide 3 by 0", result(refied.to[Future], Duration.Inf))
  }


  @Test
  def testWhile: Unit = {
    val refied = reify {
      var i = 0L
      while (!Await(Future(i < 3))) {
        i += !Await(Future(1))
      }
      i
    }
    summon[refied.type <:<
      com.thoughtworks.dsl.Dsl.Typed[
        com.thoughtworks.dsl.keywords.FlatMap[
          com.thoughtworks.dsl.keywords.While[
            com.thoughtworks.dsl.keywords.Suspend[
              com.thoughtworks.dsl.keywords.Await[Boolean]
            ]
          , 
            com.thoughtworks.dsl.keywords.Suspend[
              com.thoughtworks.dsl.keywords.FlatMap[
                com.thoughtworks.dsl.keywords.Await[Int]
              , Int, com.thoughtworks.dsl.keywords.Pure[Unit]]
            ]
          ]
        , Unit, com.thoughtworks.dsl.keywords.Pure[Long]]
      , Long]
    ]
    assertEquals(3L, result(refied.to[Future], Duration.Inf))

    assertEquals(3L, result(refied.as[Double => Future[Long]].apply(1.0), Duration.Inf))
  }

  @Test
  def testAssign: Unit = {
    object A {
      var a = "a"
    }
    val refied = reify {
      (!Await(Future(A))).a = !Await(Future("x"))
      A.a.toUpperCase
    }
    summon[refied.type <:<
      com.thoughtworks.dsl.Dsl.Typed[
       FlatMap[
         Await[A.type]
        , A.type, 
         FlatMap[
           Await[String]
          , String,Pure[String]
            ]
        ]
      , String]
    ]
    assertEquals("X", result(refied.to[Future], Duration.Inf))
  }

  @Test
  def testTyped: Unit = {
    val refied = reify {
      val i = !Await(Future("x"): Future[CharSequence])
      val r = !Await(Future(i)): AnyRef
      r
    }
    summon[refied.type <:< 
      com.thoughtworks.dsl.Dsl.Typed[
        com.thoughtworks.dsl.keywords.FlatMap[
          com.thoughtworks.dsl.keywords.Await[CharSequence]
        , CharSequence, 
          com.thoughtworks.dsl.keywords.FlatMap[
            com.thoughtworks.dsl.keywords.Await[CharSequence]
          , CharSequence, com.thoughtworks.dsl.keywords.Pure[Object]]
        ]
      , Object]
    ]
    assertEquals("x", result(refied.to[Future], Duration.Inf))
  }

  @Test
  def testPartialFunction: Unit = {
    val refied = reify {
      Future[PartialFunction[Int, String]]{
        case 1 => "1"
        case x if x < 0 => "negative"
        case _ => "default"
      }
    }

    summon[refied.type <:<
      com.thoughtworks.dsl.Dsl.Typed[
       Pure[
          Future[PartialFunction[Int, String]]
        ]
      , Future[PartialFunction[Int, String]]]
    ]
    type Id[A] = A
    assertThat(result(refied.to[Id], Duration.Inf), instanceOf(classOf[PartialFunction[_, _]]))
  }


  @Test
  def testClosure: Unit = {
    val refied = reify {
      val id = !Await(Future(identity[Int] _))
      val e = id.equals _
      !Await(Future(e))
    }
    summon[refied.type <:<
      com.thoughtworks.dsl.Dsl.Typed[
       FlatMap[
         Await[Int => Int]
        , Int => Int,Await[Any => Boolean]
        ]
      , Any => Boolean]
    ]
    assertThat(result(refied.to[Future], Duration.Inf), instanceOf(classOf[Function1[_, _]]))
  }

  @Test
  def testBangClosure: Unit = {
    val refied = reify {
      val e = (!Await(Future(identity[Int] _))).equals _
      !Await(Future(e))
    }
    summon[refied.type <:<
      com.thoughtworks.dsl.Dsl.Typed[
       FlatMap[
         FlatMap[
           Await[Int => Int]
          , Int => Int, 
           Pure[Any => Boolean]]
        , Any => Boolean,Await[Any => Boolean]]
      , Any => Boolean]
    ]
    assertThat(result(refied.to[Future], Duration.Inf), instanceOf(classOf[Function1[_, _]]))
  }

  @Test
  def testClosure2: Unit = {
    val refied = reify {
      val e = (!Await(Future((i: Int) => i))).equals _
      !Await(Future(e))
    }
    summon[refied.type <:<
      com.thoughtworks.dsl.Dsl.Typed[
       FlatMap[
         FlatMap[
           Await[Int => Int]
          , Int => Int, 
           Pure[Any => Boolean]]
        , Any => Boolean,Await[Any => Boolean]]
      , Any => Boolean]
    ]
    assertThat(result(refied.to[Future], Duration.Inf), instanceOf(classOf[Function1[_, _]]))
  }

}