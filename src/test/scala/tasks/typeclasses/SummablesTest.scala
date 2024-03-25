package tasks.typeclasses

import org.junit.Test
import u03.Sequences.*
import Sequence.*
import org.junit.Assert.assertEquals
import u04lab.Ex4Summables.sumAll
import u04lab.Ex4Summables.SummableGivenInstances.given


class SummablesTest:

  @Test def sumAllForDouble(): Unit =
    val s = Cons(1.0d, Cons(2.0d, Cons(3.0d, Cons(4.0d, Nil()))))
    assertEquals(10.0d, sumAll(s), 0)

  @Test def sumAllForString(): Unit =
    val s = Cons("A", Cons("B", Cons("C", Nil())))
    assertEquals("ABC", sumAll(s))







    
