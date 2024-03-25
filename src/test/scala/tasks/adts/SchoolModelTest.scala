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
  val courses: Sequence[Course] = Cons(pps, Cons(pcd, Nil()))
  val mirkoTeacher: Teacher = teacher(mirkoName, Cons(pps, Nil()))
  val alessandroTeacher: Teacher = teacher(alessandroName, Cons(pcd, Nil()))
  val teachers: Sequence[Teacher] = Cons(mirkoTeacher, Cons(alessandroTeacher, Nil()))
  val school: School = SchoolModuleImpl.school(teachers, courses)

  @Test def createCourse(): Unit =
    assertEquals("PPS", pps)

  @Test def createTeacher(): Unit =
    assertEquals(mirkoName, mirkoTeacher.name())
    assertEquals(Cons(pps, Nil()), mirkoTeacher.courses())

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

  @Test def addCourse(): Unit =
    val schoolAfterAdd = school.addCourse(chemical)
    val newCourses = Cons(chemical, school.courses())
    assertEquals(newCourses, schoolAfterAdd.courses())

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














    
