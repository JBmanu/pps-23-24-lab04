package tasks.adts

import org.junit.Assert.assertEquals
import org.junit.Test
//import tasks.adts.SchoolModelImpl.{Course, Teacher, School}
import tasks.adts.SchoolModelImpl.*

import u03.Sequences.Sequence.*

import u03.Optionals.Optional
import u03.Optionals.Optional.*

class SchoolModelTest:
  val course: Course = Just("PPS")
  val teacher: Teacher = TeacherImpl("Mirko", Cons(course, Nil()))

  @Test def canCreateCourse(): Unit =
    assertEquals(Just("PPS"), course)

  @Test def canCreateTeacher(): Unit =
    assertEquals("Mirko", teacher.name)
    assertEquals(Cons(course, Nil()), teacher.courses)






    
