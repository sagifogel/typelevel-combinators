package typelevel
package combinators

/** type level Map function representation
  *
  * @tparam L HList
  */
trait Map[L <: HList] {
  type Out <: HList
}

object Map {
  type Aux[L <: HList, R <: HList] = Map[L] { type Out = R }

  def apply[L <: HList](implicit M: Map[L]): Aux[L, M.Out] = M

  implicit def nil: Aux[HNil, HNil] = new Map[HNil] {
    override type Out = HNil
  }

  implicit def singleton[A, B](implicit F: A -> B): Aux[A :: HNil, B :: HNil] = new Map[A :: HNil] {
    override type Out = B :: HNil
  }

  implicit def inductive[A, B, T <: HList, R <: HList](
      implicit F: A -> B,
      M: Aux[A :: HNil, B :: HNil],
      M2: Aux[T, R]): Aux[A :: T, B :: R] =
    new Map[A :: T] {
      override type Out = B :: R
    }
}
