package tasks.ex7

import org.junit.Assert.assertEquals
import org.junit.Test
import tasks.ex7.CounterEx7Impl.*

class CounterTest:
  val initialValue = 0

  @Test def initialValueCounter(): Unit =
    assertEquals(initialValue, initialCounter())

  @Test def incCounter(): Unit =
    val incFun = inc().run(initialCounter())
    val incValue = (1, ())
    assertEquals(incValue, incFun)
