package tasks.adts

import tasks.adts.SchoolModel.SchoolModule
import u03.Optionals.Optional
import u03.Sequences.Sequence
import u03.{Optionals, Sequences}

object SchoolModelImpl extends SchoolModule:
  override type School = this.type
  override type Teacher = TeacherImpl
  override type Course = Optional[String]

  case class TeacherImpl(name: String, courses: Sequence[Course])

  extension (school: School)
    override def addTeacher(name: String): School = ???
    override def addCourse(name: String): School = ???
    override def teacherByName(name: String): Optional[Teacher] = ???
    override def courseByName(name: String): Optional[Course] = ???
    override def nameOfTeacher(teacher: Teacher): String = ???
    override def nameOfCourse(teacher: Teacher): String = ???
    override def setTeacherToCourse(teacher: Teacher, course: Course): School = ???
    override def coursesOfATeacher(teacher: Teacher): Sequences.Sequence[Course] = ???

