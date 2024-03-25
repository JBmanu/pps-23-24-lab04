package tasks.adts

import tasks.adts.SchoolModel.SchoolModule
import u03.Optionals.Optional
import u03.Optionals.Optional.*
import u03.Sequences.Sequence
import u03.Sequences.Sequence.*

import java.lang

object SchoolModelImpl extends SchoolModule:
  override type School = SchoolImpl
  override type Teacher = TeacherImpl
  override type Course = String

  case class TeacherImpl(name: String, courses: Sequence[Course])

  case class SchoolImpl(teachers: Sequence[Teacher], courses: Sequence[Course])

  extension (school: School)
    override def addTeacher(name: String): School =
      if name.isBlank then throw lang.IllegalArgumentException()
      val teachers = Cons(TeacherImpl(name, Nil()), school.teachers)
      SchoolImpl(teachers, school.courses)

    override def addCourse(name: String): School =
      if name.isBlank then throw lang.IllegalArgumentException()
      val courses = Cons(name, school.courses)
      SchoolImpl(school.teachers, courses)

    override def teacherByName(name: String): Optional[Teacher] =
      school.teachers match
        case Cons(h, t) if h.name.equals(name) => Just(h)
        case _ => Empty()
    override def courseByName(name: String): Optional[Course] = ???
    override def nameOfTeacher(teacher: Teacher): String = ???
    override def nameOfCourse(teacher: Teacher): String = ???
    override def setTeacherToCourse(teacher: Teacher, course: Course): School = ???
    override def coursesOfATeacher(teacher: Teacher): Sequence[Course] = ???

