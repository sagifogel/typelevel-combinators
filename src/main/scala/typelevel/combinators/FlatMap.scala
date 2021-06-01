package typelevel.combinators

/** type level FlatMap function representation
  *
  * @tparam L HList
  */
trait FlatMap[L <: HList] {
  type Out <: HList
}

object FlatMap {
  def apply[L <: HList](implicit M: FlatMap[L]): Aux[L, M.Out] = M

  type Aux[L <: HList, R <: HList] = FlatMap[L] { type Out = R }

  implicit def nil: Aux[HNil, HNil] = new FlatMap[HNil] {
    override type Out = HNil
  }

  implicit def singleton[A, B](implicit F: A -> (B :: HList)): Aux[A :: HList, B :: HList] = new FlatMap[A :: HList] {
    override type Out = B :: HList
  }

  implicit def inductive[A, B, T <: HList, R <: HList](
      implicit F: A -> (B :: HList),
      M: Aux[A :: HList, B :: HList],
      M2: Aux[T, R]): Aux[A :: T, B :: R] =
    new FlatMap[A :: T] {
      override type Out = B :: R
    }
}
