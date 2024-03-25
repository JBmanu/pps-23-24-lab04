package tasks.adts

import tasks.adts.SchoolModel.SchoolModule
import u03.Optionals.Optional
import u03.Optionals.Optional.*
import u03.Sequences.Sequence
import u03.Sequences.Sequence.*

import java.lang

object SchoolModuleImpl extends SchoolModule:
  override opaque type School = SchoolImpl
  override opaque type Teacher = TeacherImpl
  override opaque type Course = String

  private case class TeacherImpl(name: String, courses: Sequence[Course])

  private case class SchoolImpl(teachers: Sequence[Teacher], courses: Sequence[Course])

  def course(name: String): Course = name
  def teacher(name: String, courses: Sequence[Course]): Teacher = TeacherImpl(name, courses)
  def school(teachers: Sequence[Teacher], courses: Sequence[Course]): School = SchoolImpl(teachers, courses)

  extension (school: School)
    def teachers(): Sequence[Teacher] = school.teachers
    def courses(): Sequence[Course] = school.courses

    override def addTeacher(name: String): School =
      if name.isBlank then throw lang.IllegalArgumentException()
      val teachers = Cons(TeacherImpl(name, Nil()), school.teachers)
      SchoolImpl(teachers, school.courses)

    override def addCourse(name: Course): School =
      if name.isBlank then throw lang.IllegalArgumentException()
      val courses = Cons(name, school.courses)
      SchoolImpl(school.teachers, courses)

    override def teacherByName(name: String): Optional[Teacher] =
      filter(school.teachers)(_.name.equals(name)) match
        case Cons(h, _) => Just(h)
        case _ => Empty()

    override def courseByName(name: Course): Optional[Course] =
      filter(school.courses)(_.equals(name)) match
        case Cons(h, _) => Just(h)
        case _ => Empty()

    override def nameOfTeacher(teacher: Teacher): String = teacher.name
    override def nameOfCourse(teacher: Teacher): String = ???

    override def setTeacherToCourse(teacher: Teacher, course: Course): School = ???
    override def coursesOfATeacher(teacher: Teacher): Sequence[Course] = teacher.courses

