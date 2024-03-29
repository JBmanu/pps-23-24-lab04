package tasks

import tasks.adts.Ex1ComplexNumber.ComplexADT
import tasks.adts.Ex2SchoolModel.SchoolModel
import tasks.adts.Ex3Stacks.StackADT
import u03.Optionals.Optional
import u03.Optionals.Optional.*
import u03.Sequences.Sequence
import u03.Sequences.Sequence.*
import u04.monads.Monads.Monad
import u04lab.Ex4Summables.Summable

import scala.annotation.tailrec

object AllTask:
  object ComplexADTImpl extends ComplexADT:
    // Change assignment below: should probably define a case class and use it?
    opaque type Real = Double
    opaque type Imaginary = Double
    opaque type Complex = ComplexImpl

    case class ComplexImpl(real: Real, imaginary: Imaginary)

    def complex(re: Real, im: Imaginary): Complex = ComplexImpl(re, im)

    extension (complex: Complex)
      def re(): Real = complex.real
      def im(): Imaginary = complex.imaginary
      def sum(other: Complex): Complex =
        ComplexImpl(complex.re() + other.re(), complex.im() + other.im())
      def subtract(other: Complex): Complex =
        ComplexImpl(complex.re() - other.re(), complex.im() - other.im())

      private def spaceSign(n: Double): String =
        val sign = if complex.im() >= 0 then "+" else "-"
        s"$sign ${Math.abs(n)}"

      def asString(): String =
        complex match
          case ComplexImpl(re, 0) => s"$re"
          case ComplexImpl(0, im) => s"${im}i"
          case _                  => s"${complex.re()} ${spaceSign(complex.im())}i"


  object SchoolModelImpl extends SchoolModel:
    override opaque type School = SchoolImpl
    override opaque type Teacher = TeacherImpl
    override opaque type Course = String

    private case class TeacherImpl(name: String, courses: Sequence[Course])

    private case class SchoolImpl(teachers: Sequence[Teacher], courses: Sequence[Course])

    def course(name: String): Course = name

    def teacher(name: String): Teacher = TeacherImpl(name, Nil())

    def school(teachers: Sequence[Teacher], courses: Sequence[Course]): School = SchoolImpl(teachers, courses)

    private def throwIsBlack(s: String): Unit =
      if s.isBlank then throw IllegalArgumentException("String is Black")

    private def throwIfNotContains(contains: Boolean): Unit =
      if !contains then throw IllegalArgumentException("Not found element")

    private def throwIfContains(contains: Boolean): Unit =
      if contains then throw IllegalArgumentException("Contain same element")

    extension (teacher: Teacher)
      private def addCourse(course: Course): Teacher =
        throwIfContains(contains(teacher.courses)(_.equals(course)))
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
        throwIsBlack(name)
        throwIfContains(containsTeacher(name))
        val teachers = Cons(teacher(name), school.teachers)
        SchoolImpl(teachers, school.courses)

      override def addCourse(name: Course): School =
        throwIsBlack(name)
        throwIfContains(containsCourse(name))
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

      override def nameOfCourse(course: Course): String = course

      override def setTeacherToCourse(teacher: Teacher, course: Course): School =
        throwIfNotContains(containsCourse(course))
        teacherByName(teacher.name) match
          case Just(t) => school.substitutedTeacher(t.addCourse(course))
          case _       => throw IllegalArgumentException("Not found teacher")

      override def coursesOfATeacher(teacher: Teacher): Sequence[Course] =
        teacherByName(teacher.name) match
          case Just(a) => a.courses
          case _       => Nil()


  object StackImpl extends StackADT:
    opaque type Stack[A] = Sequence[A]

    def empty[A]: Stack[A] = Nil()

    extension [A](stack: Stack[A])
      def push(a: A): Stack[A] = Cons(a, stack)

      def pop(a: A): Optional[(A, Stack[A])] = stack match
        case Cons(h, t) => Just(h, t)
        case Nil()      => Empty()

      def asSequence(): Sequence[A] = stack


  object SummableGivenInstances:
    given Summable[Int] with
      def sum(a1: Int, a2: Int): Int = a1 + a2
      def zero: Int = 0

    given Summable[Double] with
      def sum(a1: Double, a2: Double): Double = a1 + a2
      def zero: Double = 0.0d

    given Summable[String] with
      def sum(a1: String, a2: String): String = a1 + a2
      def zero: String = ""


  object Ex5Traversable:
    trait Traversable[T[_]]:
      extension [A](a: T[A])
        def log(): Unit
      extension [A](seq: Sequence[T[A]])
        def logAll(): Unit = seq match
          case Cons(h, t) => h.log(); t.logAll()
          case _          => ()

    object TraversableOptional extends Traversable[Optional]:
      extension [A](a: Optional[A])
        override def log(): Unit = a match
          case Just(v) => println("The next element is: " + v)
          case _       => println("The next element is: Empty")

    object TraversableSequence extends Traversable[Sequence]:
      extension [A](a: Sequence[A])
        @tailrec
        override def log(): Unit = a match
          case Cons(h, t) => println("The next element is: " + h); t.log();
          case _          => ()


  object Ex6TryModel:
    enum TryImpl[A]:
      case Success(value: A)
      case Failure(exception: Throwable)

    opaque type Try[A] = TryImpl[A]

    def success[A](value: A): Try[A] = TryImpl.Success(value)
    def failure[A](exception: Throwable): Try[A] = TryImpl.Failure(exception)
    def exec[A](expression: => A): Try[A] = try success(expression) catch failure(_)

    extension [A](m: Try[A])
      def getOrElse[B >: A](other: B): B = m match
        case TryImpl.Success(value) => value
        case TryImpl.Failure(_) => other

    given Monad[Try] with
      override def unit[A](value: A): Try[A] = exec(value)
      extension [A](m: Try[A])
        override def flatMap[B](f: A => Try[B]): Try[B] =
          m match
            case TryImpl.Success(value)     => f(value)
            case TryImpl.Failure(exception) => failure(exception)
