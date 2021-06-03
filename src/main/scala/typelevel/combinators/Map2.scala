package typelevel.combinators

trait Map2[L <: HList, R <: HList] {
  type Out <: HList
}

object Map2 {
  type Aux[L <: HList, R <: HList, RR <: HList] = Map2[L, R] { type Out = RR }

  def apply[L <: HList, R <: HList](implicit M: Map2[L, R]): Aux[L, R, M.Out] = M

  implicit def nil: Aux[HNil, HNil, HNil] = new Map2[HNil, HNil] {
    override type Out = HNil
  }

  implicit def singleton[A, B, C](implicit F: (A, B) -> C): Aux[A :: HNil, B :: HNil, C :: HNil] =
    new Map2[A :: HNil, B :: HNil] {
      override type Out = C :: HNil
    }

  implicit def inductive[A, B, C, A1 <: HList, B1 <: HList, R <: HList](
      implicit F: (A, B) -> C,
      M1: Aux[A :: HNil, B :: HNil, C :: HNil],
      M2: Aux[A1, B1, R]): Aux[A :: A1, B :: B1, C :: R] =
    new Map2[A :: A1, B :: B1] {
      override type Out = C :: R
    }
}
