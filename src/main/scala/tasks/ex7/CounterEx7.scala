package tasks.ex7

import u04.monads.CounterStateImpl.counter
import u04.monads.States.*
import u04.monads.{ CounterState, CounterStateImpl, States }

trait CounterEx7 extends CounterState:
  def set(counter: Counter): State[Counter, Unit]
  def stringToCounter(s: String, default: Counter): Counter

object CounterEx7Impl extends CounterEx7:
  override type Counter = CounterStateImpl.Counter

  override def initialCounter(): Counter = CounterStateImpl.initialCounter()

  override def inc(): State[Counter, Unit] = CounterStateImpl.inc()

  override def dec(): State[Counter, Unit] = CounterStateImpl.dec()

  override def reset(): State[Counter, Unit] = CounterStateImpl.reset()

  override def get(): State[Counter, Int] = CounterStateImpl.get()

  override def nop(): State[Counter, Unit] = CounterStateImpl.nop()

  override def set(counter: Counter): State[Counter, Unit] = State(i => (counter, ()))

  override def stringToCounter(s: String, default: Counter): Counter =
    s.toIntOption match
      case Some(value) => counter(value)
      case _           => default