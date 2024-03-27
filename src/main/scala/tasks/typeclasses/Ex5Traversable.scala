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

//  private def log[A](a: A): Unit = println("The next element is: " + a)
//  private def logAll[A](seq: Sequence[A]): Unit = seq match
//    case Cons(h, t) => log(h); logAll(t)
//    case _          => ()

  trait Traversable[T[_]]:
    def log[A](a: T[A]): Unit
    def logAll[A](seq: Sequence[T[A]]): Unit

  object TraversableOptional extends Traversable[Optional]:
    override def log[A](a: Optional[A]): Unit = a match
      case Just(v) => println("The next element is: " + v)
      case _ => println("The next element is: Empty")

    @tailrec
    override def logAll[A](seq: Sequence[Optional[A]]): Unit = seq match
      case Cons(h, t) => log(h); logAll(t)
      case _ => ()

  object TraversableSequence extends Traversable[Sequence]:
    @tailrec
    override def log[A](a: Sequence[A]): Unit = a match
      case Cons(h, t) => println("The next element is: " + h); log(t);
      case _ => ()
      
    @tailrec
    override def logAll[A](seq: Sequence[Sequence[A]]): Unit = seq match
      case Cons(h, t) => log(h); logAll(t)
      case _ => ()




  
