package typelevel.combinators

/** type level val representation */
trait Val[A]

object Val {
  implicit def value[A]: Val[A] = new Val[A] {}
}
