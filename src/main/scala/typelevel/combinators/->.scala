package typelevel.combinators

/** type level Function representation */
trait ->[-A, +B]

object -> {
  implicit def function[A, B]: A -> B = new ->[A, B] {}
}
