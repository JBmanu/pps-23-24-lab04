package tasks.adts

import org.junit.Assert.{assertEquals, assertThrows}
import org.junit.Test
import tasks.adts.SchoolModelImpl.*
import u03.Sequences.Sequence
import u03.Sequences.Sequence.*
import u03.Optionals.Optional
import u03.Optionals.Optional.*

import java.lang

class SchoolModelTest:
  val pps: Course = Just("PPS")
  val pcd: Course = Just("PCD")
  val courses: Sequence[Course] = Cons(pps, Cons(pcd, Nil()))
  val mirko: Teacher = TeacherImpl("Mirko", Cons(pps, Nil()))
  val alessandro: Teacher = TeacherImpl("Alessandro", Cons(pcd, Nil()))
  val teachers: Sequence[TeacherImpl] = Cons(mirko, Cons(alessandro, Nil()))
  val school: School = SchoolImpl(teachers, courses)

  @Test def createCourse(): Unit =
    assertEquals(Just("PPS"), pps)

  @Test def createTeacher(): Unit =
    assertEquals("Mirko", mirko.name)
    assertEquals(Cons(pps, Nil()), mirko.courses)

  @Test def createSchool(): Unit =
    assertEquals(teachers, school.teachers)
    assertEquals(courses, school.courses)

  @Test def addTeacher(): Unit =
    val schoolAfterAdd = school.addTeacher("Pippo")
    val newTeachers = Cons(TeacherImpl("Pippo", Nil()), school.teachers)
    assertEquals(newTeachers, schoolAfterAdd.teachers)

  @Test def addEmptyTeacher(): Unit =
    assertThrows(classOf[IllegalArgumentException], () => school.addTeacher(""))
    assertThrows(classOf[IllegalArgumentException], () => school.addTeacher("    "))












    
