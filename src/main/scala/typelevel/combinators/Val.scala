package typelevel.combinators

trait Val[A]

object Val {
  implicit def value[A]: Val[A] = new Val[A] {}
}
