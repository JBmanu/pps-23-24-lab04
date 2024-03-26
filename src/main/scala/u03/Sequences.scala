package u03

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

@main
def trySequences =
  import Sequences.*
  val l = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(l)) // 30

  import Sequence.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
