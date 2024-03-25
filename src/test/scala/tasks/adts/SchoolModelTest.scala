package tasks.adts

import org.junit.Assert.assertEquals
import org.junit.Test
//import tasks.adts.SchoolModelImpl.{Course, Teacher, School}
import tasks.adts.SchoolModelImpl.*

import u03.Sequences.Sequence.*

import u03.Optionals.Optional
import u03.Optionals.Optional.*

class SchoolModelTest:
  val pps: Course = Just("PPS")
  val pcd: Course = Just("PCD")
  val courses = Cons(pps, Cons(pcd, Nil()))
  val mirko: Teacher = TeacherImpl("Mirko", Cons(pps, Nil()))
  val alessandro: Teacher = TeacherImpl("Alessandro", Cons(pcd, Nil()))
  val teachers = Cons(mirko, Cons(alessandro, Nil()))

  @Test def canCreateCourse(): Unit =
    assertEquals(Just("PPS"), pps)

  @Test def canCreateTeacher(): Unit =
    assertEquals("Mirko", mirko.name)
    assertEquals(Cons(pps, Nil()), mirko.courses)

  @Test def canCreateSchool(): Unit =
    val school: School = SchoolImpl(teachers, courses)
    assertEquals(teachers, school.teachers)
    assertEquals(courses, school.courses)







    
