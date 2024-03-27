package tasks.typeclasses

import org.junit.Assert.assertEquals
import org.junit.Test
import u03.Optionals.Optional.Just
import u04lab.Ex5Traversable.TraversableOptional.*

class TraversableTest:

  @Test def logOptional(): Unit =
    val opt = Just(4)
    log(opt)


