package tasks

import org.junit.Test
import org.junit.Assert.*
import u03.Sequences.Sequence
import u03.Sequences.Sequence.*
import u03.Optionals.Optional
import u03.Optionals.Optional.*

object AllTaskTest:
  class ComplexNumberTest:
    import tasks.adts.Ex1ComplexNumber.ComplexADT
    import tasks.AllTask.ComplexADTImpl
    import complexADT.*
    // Choice of implementation to test
    val complexADT: ComplexADT = ComplexADTImpl

    // From now, everything is independent of specific implementation of Complex
    @Test def testReal(): Unit =
      assertEquals(10, complex(10, 20).re(), 0)

    @Test def testImaginary(): Unit =
      assertEquals(20, complex(10, 20).im(), 0)

    @Test def testSum(): Unit =
      assertEquals(complex(11, 22), complex(10, 20) sum complex(1, 2))

    @Test def testSubtract(): Unit =
      assertEquals(complex(9, 18), complex(10, 20) subtract complex(1, 2))

    @Test def testAsString(): Unit =
      assertEquals("10.0 + 5.0i", complex(10.0, 5.0).asString())

    @Test def optionalTestAdvancedAsString(): Unit =
      assertEquals("0.0", complex(0, 0).asString())
      assertEquals("10.0", complex(10.0, 0).asString())
      assertEquals("10.0 + 5.0i", complex(10.0, 5.0).asString())
      assertEquals("10.0 - 5.0i", complex(10.0, -5.0).asString())
      assertEquals("5.0i", complex(0, 5.0).asString())
      assertEquals("-5.0i", complex(0, -5.0).asString())


  class SchoolModelTest:
    import tasks.AllTask.SchoolModelImpl
    import tasks.AllTask.SchoolModelImpl.*

    val pps: Course = course("PPS")
    val pcd: Course = course("PCD")
    val chemical: Course = course("Chemical")
    val mirkoName = "Mirko"
    val pippoName = "Pippo"
    val alessandroName = "Alessandro"
    val mirkoTeacher: Teacher = teacher(mirkoName)
    val alessandroTeacher: Teacher = teacher(alessandroName)
    val teachers: Sequence[Teacher] = Cons(mirkoTeacher, Cons(alessandroTeacher, Nil()))
    val courses: Sequence[Course] = Cons(pps, Cons(pcd, Nil()))
    val school: School = SchoolModelImpl.school(teachers, courses)

    @Test def createCourse(): Unit =
      assertEquals("PPS", pps)

    @Test def createSchool(): Unit =
      assertEquals(teachers, school.teachers())
      assertEquals(courses, school.courses())

    @Test def addTeacher(): Unit =
      val schoolAfterAdd = school.addTeacher(pippoName)
      val newTeachers = Cons(teacher(pippoName), school.teachers())
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
      assertEquals(Nil(), courses)

    @Test def nameOfCourse(): Unit =
      val nameOfCourse = school.nameOfCourse(mirkoTeacher)
      assertEquals("", nameOfCourse)

    @Test def setTeacherToCourse(): Unit = {
      val newSchool = school.setTeacherToCourse(mirkoTeacher, pps)
      val coursesOfATeacher = newSchool.coursesOfATeacher(mirkoTeacher)
      assertEquals(Cons(pps, Nil()), coursesOfATeacher)
    }

    @Test def setTeacherToCourseNotOfSchool(): Unit = {
      assertThrows(classOf[IllegalArgumentException], () => school.setTeacherToCourse(mirkoTeacher, chemical))
    }

    @Test def setTeacherNotOfSchoolToCourse(): Unit = {
      val pippoTeacher: Teacher = teacher(pippoName)
      assertThrows(classOf[IllegalArgumentException], () => school.setTeacherToCourse(pippoTeacher, pps))
    }


  class Stacktest:
    import tasks.AllTask.StackImpl
    val stack = StackImpl

    import stack.*

    @Test def testEmpty(): Unit =
      assertEquals(Sequence.Nil(), empty[Int].asSequence())

    @Test def testPush(): Unit =
      assertEquals(Sequence.Cons(10, Sequence.Nil()), empty[Int].push(10).asSequence())

    @Test def testPopOnEmpty(): Unit =
      assertEquals(Optional.Empty(), empty[Int].pop(10))

    @Test def testPopOnNotEmpty(): Unit =
      assertEquals(Optional.Just((10, Sequence.Nil())), empty[Int].push(10).pop(10))


  class SummableGivenTest:
    import u04lab.Ex4Summables.sumAll
    import tasks.AllTask.SummableGivenInstances.given

    @Test def testSummableGivenInt(): Unit =
      val si = Cons(10, Cons(20, Cons(30, Nil())))
      assertEquals(60, sumAll(si))

    @Test def testSummableGivenDouble(): Unit =
      val sd = Cons(10.0, Cons(20.0, Cons(30.0, Nil())))
      assertEquals(60.0d, sumAll(sd), 0.0d)

    @Test def testSummableGivenString(): Unit =
      val ss = Cons("10", Cons("20", Cons("30", Nil())))
      assertEquals("102030", sumAll(ss))


  class TraversableOptionalTest:
    import tasks.AllTask.Ex5Traversable.TraversableOptional.*

    @Test def logOptional(): Unit =
      val opt = Just(4)
      opt.log()

    @Test def logSequenceOfOptional(): Unit =
      val seq = Cons(Just(0), Cons(Just(1), Nil()))
      seq.logAll()
      val seq1 = Cons(Just(0), Cons(Empty(), Nil()))
      seq1.logAll()

  class TraversableSequenceTest:
    import tasks.AllTask.Ex5Traversable.TraversableSequence.*

    val seq: Sequence[Int] = Cons(1, Cons(2, Cons(3, Nil())))

    @Test def logSequence(): Unit =
      seq.log()

    @Test def logSequenceOfSequence(): Unit =
      val seq1 = Cons(4, Cons(5, Cons(6, Nil())))
      val seqOfSeq = Cons(seq, Cons(seq1, Nil()))
      seqOfSeq.logAll()


  class TryModelTest:
    import tasks.AllTask.Ex6TryModel.*

    @Test def monadSuccess(): Unit =
      val result = for
        a <- success(10)
        b <- success(30)
      yield a + b
      assert(result.getOrElse(-1) == 40)

    @Test def monadOneFailure(): Unit =
      val result2 = for
        a <- success(10)
        b <- failure(new RuntimeException("error"))
        c <- success(30)
      yield a + c

      assert(success(20).map(_ + 10).getOrElse(-1) == 30)
      assert(result2.getOrElse(-1) == -1)

    @Test def monadExec(): Unit =
      val result3 = for
        a <- exec(10)
        b <- exec(throw new RuntimeException("error"))
        c <- exec(30)
      yield a + c
      assert(result3.getOrElse(-1) == -1)
