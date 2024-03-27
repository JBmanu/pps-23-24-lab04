package tasks.typeclasses

import org.junit.Assert.assertEquals
import org.junit.Test
import u03.Optionals.Optional.*

import u03.Sequences.*
import u03.Sequences.Sequence.*

class TraversableOptionalTest:
  import u04lab.Ex5Traversable.TraversableOptional.*
  @Test def logOptional(): Unit =
    val opt = Just(4)
    log(opt)

  @Test def logSequenceOfOptional(): Unit =
    val seq = Cons(Just(0), Cons(Just(1), Nil()))
    logAll(seq)
    val seq1 = Cons(Just(0), Cons(Empty(), Nil()))
    logAll(seq1)

class TraversableSequenceTest:
  import u04lab.Ex5Traversable.TraversableSequence.*
  @Test def logSequence(): Unit =
    val seq = Cons(1, Cons(2, Cons(3, Nil())))
    log(seq)


