package typelevel.combinators

/** type level product function representation
  *
  * @tparam L HList
  * @tparam R HList
  */
trait Product[L <: HList, R <: HList] {
  type Out <: HList
}

object Product {
  type Aux[L <: HList, R <: HList, RR <: HList] = Product[L, R] { type Out = RR }

  def apply[L <: HList, R <: HList](implicit M: Product[L, R]): Aux[L, R, M.Out] = M

  implicit def nil: Aux[HNil, HNil, HNil] = new Product[HNil, HNil] {
    override type Out = HNil
  }

  implicit def singleton[A, B, C]: Aux[A :: HNil, B :: HNil, (A, B) :: HNil] =
    new Product[A :: HNil, B :: HNil] {
      override type Out = (A, B) :: HNil
    }

  implicit def inductive[A, B, A1 <: HList, B1 <: HList, R <: HList](
      implicit M1: Aux[A :: HNil, B :: HNil, (A, B) :: HNil],
      M2: Aux[A1, B1, R]): Aux[A :: A1, B :: B1, (A, B) :: R] =
    new Product[A :: A1, B :: B1] {
      override type Out = (A, B) :: R
    }
}
