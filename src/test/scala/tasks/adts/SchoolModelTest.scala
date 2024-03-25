package tasks.adts

import org.junit.Assert.{assertEquals, assertThrows}
import org.junit.Test
import tasks.adts.SchoolModelImpl.*
import u03.Optionals.Optional
import u03.Optionals.Optional.*
import u03.Sequences.Sequence
import u03.Sequences.Sequence.*

import java.lang

class SchoolModelTest:
  val pps: Course = "PPS"
  val pcd: Course = "PCD"
  val courses: Sequence[Course] = Cons(pps, Cons(pcd, Nil()))
  val teacherMirko: Teacher = TeacherImpl("Mirko", Cons(pps, Nil()))
  val teacherAlessandro: Teacher = TeacherImpl("Alessandro", Cons(pcd, Nil()))
  val teachers: Sequence[TeacherImpl] = Cons(teacherMirko, Cons(teacherAlessandro, Nil()))
  val school: School = SchoolImpl(teachers, courses)

  @Test def createCourse(): Unit =
    assertEquals("PPS", pps)

  @Test def createTeacher(): Unit =
    assertEquals("Mirko", teacherMirko.name)
    assertEquals(Cons(pps, Nil()), teacherMirko.courses)

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

  @Test def addCourse(): Unit =
    val schoolAfterAdd = school.addCourse("Chemical")
    val newCourses = Cons("Chemical", school.courses)
    assertEquals(newCourses, schoolAfterAdd.courses)

  @Test def addEmptyCourse(): Unit =
    assertThrows(classOf[IllegalArgumentException], () => school.addCourse(""))
    assertThrows(classOf[IllegalArgumentException], () => school.addCourse("    "))

  @Test def searchTeacherByName(): Unit =
    val searchMirko = school.teacherByName("Mirko")
    assertEquals(Just(teacherMirko), searchMirko)











    
