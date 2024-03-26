package u03

import u03.Optionals.Optional
import u03.Optionals.Optional.*

object Sequences: // Essentially, generic linkedlists

  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _          => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil()      => Nil()

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t)            => filter(t)(pred)
      case Nil()                 => Nil()

    def contains[A](l: Sequence[A])(pred: A => Boolean): Boolean =
      filter(l)(pred) match
        case Cons(h, _) => true
        case _          => false

    def remove[A](l: Sequence[A])(pred: A => Boolean): Sequence[A] =
      l match
        case Cons(h, t) if !pred(h) => Cons(h, remove(t)(pred))
        case _                      => Nil()

    def substituted[A](l: Sequence[A])(pred: A => Boolean)(value: A): Sequence[A] =
      l match
        case Cons(h, t) if pred(h) => Cons(value, substituted(t)(pred)(value))
        case Cons(h, t)            => Cons(h, substituted(t)(pred)(value))
        case _                     => Nil()

    def findFirst[A](l: Sequence[A])(pred: A => Boolean): Optional[A] =
      filter(l)(pred) match
        case Cons(h, t) => Just(h)
        case _ => Empty()

    def concat[A](l1: Sequence[A])(l2: Sequence[A]): Sequence[A] =
      l1 match
        case Cons(h, t) => Cons(h, concat(t)(l2))
        case _ => l2

    def toString[A](l: Sequence[A]): String =
      l match
        case Cons(h, t) => s"$h ${toString(t)}".strip()
        case _ => ""


@main
def trySequences =
  import Sequences.*
  val l = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(l)) // 30

  import Sequence.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
