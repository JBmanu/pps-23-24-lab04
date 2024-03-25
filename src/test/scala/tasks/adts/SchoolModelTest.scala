package tasks.adts

import org.junit.Assert.assertEquals
import org.junit.Test
import tasks.adts.SchoolModelImpl.Course
import u03.Optionals.Optional
import u03.Optionals.Optional.*

class SchoolModelTest:

  @Test def canCreateCourse(): Unit =
    val course: Course = Just("PPS")
    assertEquals(Just("PPS"), course)
    
  

    
