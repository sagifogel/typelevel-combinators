package typelevel.combinators

/** type level zipWithIndex function representation
  *
  * @tparam L HList
  */
trait ZipWithIndex[L <: HList] {
  type Out
}

object ZipWithIndex {
  type Aux[L <: HList, R] = ZipWithIndex[L] { type Out = R }

  def apply[L <: HList](implicit Z: ZipWithIndex[L]): Aux[L, Z.Out] = Z

  implicit def nil: Aux[HNil, HNil] = new ZipWithIndex[HNil] {
    override type Out = HNil
  }

  implicit def singleton[A]: Aux[A :: HNil, (A, Int) :: HNil] = new ZipWithIndex[A :: HNil] {
    override type Out = (A, Int) :: HNil
  }

  implicit def inductive[A, L <: HList, R <: HList](
      implicit FM: Aux[A :: HNil, (A, Int) :: HNil],
      FM2: Aux[L, R]): Aux[A :: L, (A, Int) :: R] =
    new ZipWithIndex[A :: L] {
      override type Out = (A, Int) :: R
    }

}
