package tasks.adts

import org.junit.Assert.{assertEquals, assertThrows}
import org.junit.Test
import tasks.adts.SchoolModuleImpl.*
import u03.Optionals.Optional
import u03.Optionals.Optional.*
import u03.Sequences.Sequence
import u03.Sequences.Sequence.*

import java.lang

class SchoolModelTest:
  val pps: Course = course("PPS")
  val pcd: Course = course("PCD")
  val chemical: Course = course("Chemical")
  val mirkoName = "Mirko"
  val pippoName = "Pippo"
  val alessandroName = "Alessandro"
  val mirkoTeacher: Teacher = teacher(mirkoName, Cons(pps, Nil()))
  val alessandroTeacher: Teacher = teacher(alessandroName, Cons(pcd, Nil()))
  val teachers: Sequence[Teacher] = Cons(mirkoTeacher, Cons(alessandroTeacher, Nil()))
  val courses: Sequence[Course] = Cons(pps, Cons(pcd, Nil()))
  val school: School = SchoolModuleImpl.school(teachers, courses)

  @Test def createCourse(): Unit =
    assertEquals("PPS", pps)

  @Test def createSchool(): Unit =
    assertEquals(teachers, school.teachers())
    assertEquals(courses, school.courses())

  @Test def addTeacher(): Unit =
    val schoolAfterAdd = school.addTeacher(pippoName)
    val newTeachers = Cons(teacher(pippoName, Nil()), school.teachers())
    assertEquals(newTeachers, schoolAfterAdd.teachers())

  @Test def addEmptyTeacher(): Unit =
    val emptyCourse = ""
    val emptyCourseWithBlank = "    "
    assertThrows(classOf[IllegalArgumentException], () => school.addTeacher(emptyCourse))
    assertThrows(classOf[IllegalArgumentException], () => school.addTeacher(emptyCourseWithBlank))

  @Test def addSameTeacherTwice(): Unit =
    val schoolAfterFirstAdd = school.addTeacher(pippoName)
    assertThrows(classOf[IllegalArgumentException], () => schoolAfterFirstAdd.addTeacher(pippoName))

  @Test def addCourse(): Unit =
    val schoolAfterAdd = school.addCourse(chemical)
    val newCourses = Cons(chemical, school.courses())
    assertEquals(newCourses, schoolAfterAdd.courses())

  @Test def addSameCourseTwice(): Unit =
    val schoolAfterFirstAdd = school.addCourse(chemical)
    assertThrows(classOf[IllegalArgumentException], () => schoolAfterFirstAdd.addCourse(chemical))

  @Test def addEmptyCourse(): Unit =
    val emptyCourse = course("")
    val emptyCourseWithBlank = course("    ")
    assertThrows(classOf[IllegalArgumentException], () => school.addCourse(emptyCourse))
    assertThrows(classOf[IllegalArgumentException], () => school.addCourse(emptyCourseWithBlank))

  @Test def searchTeacherByName(): Unit =
    val searchMirko = school.teacherByName(alessandroName)
    assertEquals(Just(alessandroTeacher), searchMirko)

  @Test def searchTeacherNotInSchool(): Unit =
    val searchPippo = school.teacherByName(pippoName)
    assertEquals(Empty(), searchPippo)

  @Test def searchCourseByName(): Unit =
    val searchPCD = school.courseByName(pcd)
    assertEquals(Just(pcd), searchPCD)

  @Test def searchCourseNotInSchool(): Unit =
    val searchChemical = school.courseByName(chemical)
    assertEquals(Empty(), searchChemical)

  @Test def nameOfTeacher(): Unit =
    val name = school.nameOfTeacher(mirkoTeacher)
    assertEquals(mirkoName, name)

  @Test def coursesOfATeacher(): Unit =
    val courses = school.coursesOfATeacher(mirkoTeacher)
    assertEquals(Cons(pps, Nil()), courses)

  @Test def nameOfCourse(): Unit =
    val nameOfCourse = school.nameOfCourse(mirkoTeacher)
    assertEquals(pps, nameOfCourse)

//  @Test def setTeacherToCourse(): Unit = {
//    val oop = course("OOP")
//    val newSchool = school.setTeacherToCourse(mirkoTeacher, oop)
//    val nameOfCourse = newSchool.nameOfCourse(mirkoTeacher)
//    assertEquals(oop, nameOfCourse)
//  }