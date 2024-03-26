package u03

import org.junit.*
import org.junit.Assert.*
import u03.Optionals.Optional.*

class SequenceTest:

  import u03.Sequences.*
  import Sequence.*

  val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum(): Unit =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  @Test def testMap(): Unit =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_ + ""))

  @Test def testFilter(): Unit =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_ != 20))

  @Test def testContains(): Unit =
    assertTrue(contains(l)(_.equals(30)))
    assertFalse(contains(l)(_.equals(40)))

  @Test def testRemove(): Unit =
    val sequenceMatch: Sequence[Int] = Cons(10, Cons(20, Nil()))
    assertEquals(sequenceMatch, remove(l)(_.equals(30)))
    assertEquals(l, remove(l)(_.equals(40)))

  @Test def testSubstituted(): Unit =
    val sequenceMatch: Sequence[Int] = Cons(10, Cons(20, Cons(40, Nil())))
    assertEquals(sequenceMatch, substituted(l)(_.equals(30))(40))
    assertEquals(l, substituted(l)(_.equals(40))(50))

  @Test def testFind(): Unit =
    val value = Just(30)
    assertEquals(value, findFirst(l)(_.equals(30)))
    assertEquals(Empty(), findFirst(l)(_.equals(40)))

