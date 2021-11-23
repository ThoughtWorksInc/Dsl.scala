package com.thoughtworks.dsl
package keywords
import Dsl.IsKeyword
import Dsl.Typed


opaque type In[Element] <: Any = Iterable[Element]
object In {
  @inline def cast[Element]: Iterable[Element] <:< In[Element] = implicitly
  def apply[Element](iterable: Iterable[Element]): In[Element] = iterable

  given[Element]: IsKeyword[In[Element], Element]

  given[Element, Domain, DomainElement](
    given
    conversion: Domain => IterableOnce[DomainElement],
    factory: collection.Factory[DomainElement, Domain],
  ): Dsl[In[Element], Domain, Element] {
    def cpsApply(keyword: In[Element], handler: Element => Domain): Domain = {
      factory.fromSpecific(new collection.View.FlatMap(keyword, handler.andThen(conversion)))
    }
  }

  private class OptimizedDrop[Element](
    underlying: collection.SeqOps[Element, [x] =>> Any, _],
    n: Int
  ) extends collection.SeqView.Drop[Element](
    underlying,
    n
  ) {
    override def drop(n: Int): OptimizedDrop[Element] = new OptimizedDrop(underlying, this.n + n)
  }

  private type SeqOrSeqView[A] = collection.Seq[A] | OptimizedDrop[A]
  private def toLinearSeqOps[Element](i: IterableOnce[Element]): SeqOrSeqView[Element] = {
    i match {
      case linearSeq: collection.LinearSeq[Element] =>
        linearSeq
      case indexedSeq: collection.IndexedSeq[Element] =>
        new OptimizedDrop(indexedSeq, 0)
      case notSeq =>
        List.from(notSeq)
    }
  }

  import Dsl.Typed

  // 这种方式要么增加运行时type class复杂度，要么需要修改AST而很难支持 for / yield
  // given[Element, MappedKeyword, Domain, MappedContainer[x] <: IterableOnce[x], MappedElement, PureValue](
  //   given
  //   factory: collection.Factory[MappedElement, MappedContainer[MappedElement]],
  //   dsl: Dsl[MappedKeyword, Domain, MappedContainer[MappedElement]]
  // ): Dsl[
  //   FlatMap[
  //     In[Element],
  //     Element,
  //     MappedKeyword,
  //     PureValue
  //   ],
  //   Domain,
  //   MappedContainer[MappedElement]
  // ] {
  //   def (
  //     keyword:
  //     FlatMap[
  //       In[Element],
  //       Element,
  //       MappedKeyword,
  //       MappedContainer[MappedElement]
  //     ]
  //   )cpsApply(handler:  MappedContainer[MappedElement] => Domain): Domain = {
  //     val flatMapper = keyword.flatMapper
  //     def loop(seqOps: SeqOrSeqView[Element], accumulator: List[MappedContainer[MappedElement]]): Domain = {
  //       seqOps.headOption match {
  //         case None =>
  //           handler(factory.fromSpecific(accumulator.reverseIterator.flatten))
  //         case Some(head) =>
  //           // Workaround for https://github.com/lampepfl/dotty/issues/7880
  //           val tail = seqOps.tail.asInstanceOf[SeqOrSeqView[Element]]
  //           dsl.cpsApply(Typed.cast.flip(flatMapper(head))) { (mapped: MappedContainer[MappedElement]) =>
  //             loop(tail, mapped :: accumulator)
  //           }
  //       }
  //       // keyword.flatMapper()
  //       // ???
  //     }
  //     loop(toLinearSeqOps(Typed.cast.flip(keyword.upstream)), Nil)
  //     // keyword.flatMapper
  //     // ???
  //   }
  // }

  import Dsl.!!
  given[Element, Output[DomainElement], OuterDomain, DomainElement](
    given
    conversion: Output[DomainElement] => IterableOnce[DomainElement],
    // lift: Lift[Output[DomainElement], OuterDomain],
    factory: collection.Factory[DomainElement, Output[DomainElement]],
  ): Dsl[In[Element], OuterDomain !! Output[DomainElement], Element] {
    // type InnerDomain = OuterDomain !! Output[DomainElement]
    def cpsApply(
      keyword: In[Element],
      handler: Element => OuterDomain !! Output[DomainElement]
    ): OuterDomain !! Output[DomainElement] = { join =>
      @inline def loop(
        seqOps: SeqOrSeqView[Element],
        accumulator: List[Output[DomainElement]]
      ): OuterDomain = {
        seqOps.headOption match {
          case None =>
            join(factory.fromSpecific(accumulator.view.reverse.flatMap(conversion)))
          case Some(head) =>
            // Workaround for https://github.com/lampepfl/dotty/issues/7880
            val tail: SeqOrSeqView[Element] = seqOps.drop(1).asInstanceOf[SeqOrSeqView[Element]]
            handler(head) { (expended: Output[DomainElement]) =>
              loop(tail, expended :: accumulator)
            }
        }
      }
      loop(toLinearSeqOps(keyword), Nil)
      // keyword.view.map(handler).reduce(???)
      // match {
      //   case (head: Element) +: tail =>
      //     handler(head) ++ tail.cpsApply(handler)
      // }
      // val i = keyword.elements.toIterator
      // val builder = newBuilder[DomainElement, RightDomain]
      // val handler = rightDomainIsTraversableOnce(handler0)
      // @inline
      // def loop(continue: RightDomain => LeftDomain): LeftDomain = {
      //   if (i.hasNext) {
      //     builder ++= !handler(i.next())
      //     loop(continue)
      //   } else {
      //     continue(builder.result())
      //   }
      // }
      // loop

      // ???
    }
  }

  // given[
  //   Element,
  //   ViewDomain
  //     >: collection.View.FlatMap[Element, DomainElement]
  //     <: IterableOnce[DomainElement], DomainElement
  // ]: Dsl[In[Element], ViewDomain, Element] {
  //   type InnerDomain = IterableOnce[DomainElement]
  //   def(keyword: In[Element])cpsApply(handler: Element => InnerDomain): ViewDomain = {
  //     new collection.View.FlatMap(keyword, handler)
  //   }
  // }
}
