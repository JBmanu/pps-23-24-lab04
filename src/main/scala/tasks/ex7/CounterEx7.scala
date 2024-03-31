package tasks.ex7

import u04.monads.States.*
import u04.monads.{ CounterState, States }
import u04.monads.CounterStateImpl

trait CounterEx7 extends CounterState:
  extension (value: Int)
    def set(): State[Counter, Unit]

object CounterEx7Impl extends CounterEx7:
  override type Counter = CounterStateImpl.Counter

  override def initialCounter(): Counter = CounterStateImpl.initialCounter()

  override def inc(): State[Counter, Unit] = CounterStateImpl.inc()

  override def dec(): State[Counter, Unit] = CounterStateImpl.dec()

  override def reset(): State[Counter, Unit] = CounterStateImpl.reset()

  override def get(): State[Counter, Int] = CounterStateImpl.get()

  override def nop(): State[Counter, Unit] = CounterStateImpl.nop()

  extension (value: Int)
    override def set(): State[Counter, Unit] = ???