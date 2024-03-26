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
  def teacher(name: String, courses: Sequence[Course]): Teacher = TeacherImpl(name, courses)
  def school(teachers: Sequence[Teacher], courses: Sequence[Course]): School = SchoolImpl(teachers, courses)

  extension (teacher: Teacher)
    def addCourse(course: Course): Teacher =
      if contains(teacher.courses)(_.equals(course)) then throw IllegalArgumentException("Contain same course")
      teacher match
        case TeacherImpl(n, c) => TeacherImpl(n, Cons(course, c))

  extension (school: School)
    def teachers(): Sequence[Teacher] = school.teachers
    def courses(): Sequence[Course] = school.courses

    private def containsTeacher(teacherName: String): Boolean =
      contains(school.teachers)(_.name.equals(teacherName))

    private def containsCourse(name: Course): Boolean =
      contains(school.courses)(_.equals(name))

    override def addTeacher(name: String): School =
      if name.isBlank then throw IllegalArgumentException("Empty Name")
      if containsTeacher(name) then throw IllegalArgumentException("Contain same teacher")
      val teachers = Cons(TeacherImpl(name, Nil()), school.teachers)
      SchoolImpl(teachers, school.courses)

    override def addCourse(name: Course): School =
      if name.isBlank then throw IllegalArgumentException("Empty Course")
      if containsCourse(name) then throw IllegalArgumentException("Contain same course")
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

    private def findFistTeacher(teacher: Teacher): Optional[Teacher] =
      findFirst(school.teachers)(_.name.equals(teacher.name))

    override def nameOfTeacher(teacher: Teacher): String =
      findFistTeacher(teacher) match
        case Just(TeacherImpl(n, c)) => s"$n"
        case _ => ""

    override def nameOfCourse(teacher: Teacher): String =
      findFistTeacher(teacher) match
        case Just(TeacherImpl(n, c)) => s"$c"
        case _ => ""

    override def setTeacherToCourse(teacher: Teacher, course: Course): School =
      if containsCourse(course) then throw IllegalArgumentException("Not Found Course")
      findFirst(school.teachers)(_.name.equals(teacher.name)) match
        case Just(t) =>
          val newTeachers = substituted(school.teachers)(_.name.equals(teacher.name))(t.addCourse(course))
          println(s"$newTeachers")
          SchoolImpl(newTeachers, school.courses)
        case _ => throw IllegalArgumentException("Not found teacher")

    override def coursesOfATeacher(teacher: Teacher): Sequence[Course] = teacher.courses

