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

  @Test def decCounter(): Unit =
    val decFun = dec().run(initialCounter())
    val decValue = (-1, ())
    assertEquals(decValue, decFun)

  @Test def resetCounter(): Unit = 
    val incInitialCounter = inc().run(initialCounter())._1
    val resetFun = reset().run(incInitialCounter)
    val resetValue = (0, ())
    assertEquals(resetValue, resetFun)