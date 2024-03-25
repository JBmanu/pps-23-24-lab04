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

    private def containsTeacher(teacherName: String): Boolean =
      filter(school.teachers)(_.name.equals(teacherName)) match
        case Cons(h, _) => true
        case _ => false

    override def addTeacher(name: String): School =
      if name.isBlank then throw IllegalArgumentException("String Empty")
      if containsTeacher(name) then throw IllegalArgumentException("Contain same teacher")
      val teachers = Cons(TeacherImpl(name, Nil()), school.teachers)
      SchoolImpl(teachers, school.courses)

    override def addCourse(name: Course): School =
      if name.isBlank then throw IllegalArgumentException("String Empty")
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

    override def nameOfCourse(teacher: Teacher): String =
      teacher.courses match
        case Cons(h, _) => h
        case _ => ""

    override def setTeacherToCourse(teacher: Teacher, course: Course): School = ???
//      val newTeacher = TeacherImpl(teacher.name, Cons(course, Nil()))
//      school.addTeacher()
//      val filterNotEquals = filter(school.teachers)(!_.equals(teacher))
//      var filterEquals = filter(school.teachers)(_.equals(teacher))
//      filterEquals match
//        case Cons(h, _) =>
//          val newTeacher = Cons(TeacherImpl(h.name, Cons(course, Nil())), filterNotEquals)
//          val newSchool = SchoolImpl(newTeacher, school.courses)
//          newSchool.addCourse(course)
//          newSchool
//        case _ => "ERRORE non ce prof"

    override def coursesOfATeacher(teacher: Teacher): Sequence[Course] = teacher.courses

