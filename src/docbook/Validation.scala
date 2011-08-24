trait Semigroup[A] {
  // associative
  def append(a1: A, a2: A): A
}

object Semigroup {
  implicit def ListSemigroup[A]: Semigroup[List[A]] = new Semigroup[List[A]] {
    def append(a1: List[A], a2: List[A]) = a1 ::: a2
  }
}

sealed trait Validation[E, X] {
  def map[Y](f: X => Y): Validation[E, Y]
    = this match {
    case Failure(e) => Failure(e)
    case Success(x) => Success(f(x))
  }

  // applicative functor
  def <<*>>[Y](f: Validation[E, X => Y])
            (implicit s: Semigroup[E]): Validation[E, Y]
    = (this, f) match {
      case (Failure(e1), Failure(e2)) => Failure(s append (e1, e2))
      case (Failure(e1), Success(_))  => Failure(e1)
      case (Success(_), Failure(e2))  => Failure(e2)
      case (Success(x), Success(k))   => Success(k(x))
    }
}
final case class Failure[E, X](e: E) extends Validation[E, X]
final case class Success[E, X](x: X) extends Validation[E, X]

////

// Age must be between 0 and 130
// Name must start with upper-case character
// Postcode must be 4 digits
case class Person(age: Int, name: String, postcode: String)

object Person {
  def validAge(s: String): Validation[List[String], Int] =
    try {
      val a = s.toInt
      if(a < 0)
        Failure(List("Age must be greater than 0"))
      else if(a > 130)
        Failure(List("Age must be less than 130"))
      else
        Success(a)
    } catch {
      case e => Failure(List(e.toString))
    }

  def validName(s: String): Validation[List[String], String] =
    if(s.headOption exists (_.isUpper))
      Success(s)
    else
      Failure(List("Name must begin with a capital letter"))

  def validPostcode(s: String): Validation[List[String], String] =
    if(s.length == 4 && s.forall(_.isDigit))
      Success(s)
    else
      Failure(List("Postcode must be 4 digits"))

   def main(args: Array[String]) {
      if(args.length < 3)
        println("Need at least three arguments")
      else {
        val f = (Person(_, _, _)).curried
        val age = validAge(args(0))
        val name = validName(args(1))
        val postcode = validPostcode(args(2))

        postcode <<*>> (name <<*>> (age map f)) match {
          case Success(p) => println("We have a person: " + p)
          case Failure(e) => e foreach println
        }
      }
   }
}

/*
$ scala Person 30 Fred 4000
We have a person: Person(30,Fred,4000)

$ scala Person "-7" red 490000
Postcode must be 4 digits
Name must begin with a capital letter
Age must be greater than 0

$ scala Person boo Fred 490000
Postcode must be 4 digits
java.lang.NumberFormatException: For input string: "boo"

$ scala Person 30 "" 411x
Postcode must be 4 digits
Name must begin with a capital letter
*/
