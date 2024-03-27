package tasks.typeclasses

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
  
  val seq: Sequence[Int] = Cons(1, Cons(2, Cons(3, Nil())))
  
  @Test def logSequence(): Unit =
    log(seq)
    
  @Test def logSequenceOfSequence(): Unit =
    val seq1 = Cons(4, Cons(5, Cons(6, Nil())))
    val seqOfSeq = Cons(seq, Cons(seq1, Nil()))
    logAll(seqOfSeq)


