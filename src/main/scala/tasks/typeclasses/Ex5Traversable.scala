package u04lab

import u03.Optionals.Optional
import u03.Optionals.Optional.*
import u03.Sequences.*
import u03.Sequences.Sequence.*

import scala.annotation.tailrec

/*  Exercise 5: 
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others... 
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a datastructure T[A] 
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

object Ex5Traversable:

  trait Traversable[T[_]]:
    extension [A](a: T[A])
      def log(): Unit
    extension [A](seq: Sequence[T[A]])
      def logAll(): Unit = seq match
        case Cons(h, t) => h.log(); t.logAll()
        case _          => ()

  object TraversableOptional extends Traversable[Optional]:
    extension [A](a: Optional[A])
      override def log(): Unit = a match
        case Just(v) => println("The next element is: " + v)
        case _ => println("The next element is: Empty")

  object TraversableSequence extends Traversable[Sequence]:
    extension [A](a: Sequence[A])
      @tailrec
      override def log(): Unit = a match
        case Cons(h, t) => println("The next element is: " + h); t.log();
        case _          => ()
