package tasks.adts

import tasks.adts.SchoolModel.SchoolModel
import u03.Optionals.Optional
import u03.Optionals.Optional.*
import u03.Sequences.Sequence
import u03.Sequences.Sequence.*

import java.lang

object SchoolModelImpl extends SchoolModel:
  override opaque type School = SchoolImpl
  override opaque type Teacher = TeacherImpl
  override opaque type Course = String

  private case class TeacherImpl(name: String, courses: Sequence[Course])

  private case class SchoolImpl(teachers: Sequence[Teacher], courses: Sequence[Course])

  def course(name: String): Course = name

  def teacher(name: String): Teacher = TeacherImpl(name, Nil())

  def school(teachers: Sequence[Teacher], courses: Sequence[Course]): School = SchoolImpl(teachers, courses)

  extension (teacher: Teacher)
    private def addCourse(course: Course): Teacher =
      if contains(teacher.courses)(_.equals(course)) then throw IllegalArgumentException("Contain same course")
      teacher match
        case TeacherImpl(n, c) => TeacherImpl(n, Cons(course, c))

  extension (school: School)
    def teachers(): Sequence[Teacher] = school.teachers
    def courses(): Sequence[Course] = school.courses

    private def containsTeacher(teacherName: String): Boolean =
      !isEmpty(school.teacherByName(teacherName))

    private def containsCourse(name: Course): Boolean =
      !isEmpty(school.courseByName(name))

    private def substitutedTeacher(teacher: Teacher): School =
      SchoolImpl(substituted(school.teachers)(_.name.equals(teacher.name))(teacher),
                 school.courses)

    override def addTeacher(name: String): School =
      if name.isBlank then throw IllegalArgumentException("Empty Name")
      if containsTeacher(name) then throw IllegalArgumentException("Contain same teacher")
      val teachers = Cons(teacher(name), school.teachers)
      SchoolImpl(teachers, school.courses)

    override def addCourse(name: Course): School =
      if name.isBlank then throw IllegalArgumentException("Empty Course")
      if containsCourse(name) then throw IllegalArgumentException("Contain same course")
      val courses = Cons(name, school.courses)
      SchoolImpl(school.teachers, courses)

    override def teacherByName(name: String): Optional[Teacher] =
      filter(school.teachers)(_.name.equals(name)) match
        case Cons(h, _) => Just(h)
        case _          => Empty()

    override def courseByName(name: Course): Optional[Course] =
      filter(school.courses)(_.equals(name)) match
        case Cons(h, _) => Just(h)
        case _          => Empty()

    override def nameOfTeacher(teacher: Teacher): String =
      teacherByName(teacher.name) match
        case Just(TeacherImpl(n, c)) => s"$n"
        case _                       => ""

    override def nameOfCourse(teacher: Teacher): String =
      teacherByName(teacher.name) match
        case Just(TeacherImpl(n, c)) => Sequence.toString(c)
        case _                       => ""

    override def setTeacherToCourse(teacher: Teacher, course: Course): School =
      if !containsCourse(course) then throw IllegalArgumentException("Not Found Course")
      teacherByName(teacher.name) match
        case Just(t) => school.substitutedTeacher(t.addCourse(course))
        case _       => throw IllegalArgumentException("Not found teacher")

    override def coursesOfATeacher(teacher: Teacher): Sequence[Course] =
      teacherByName(teacher.name) match
        case Just(a) => a.courses
        case _       => Nil()
