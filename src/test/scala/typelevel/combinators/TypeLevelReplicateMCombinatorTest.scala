package typelevel.combinators

import typelevel.combinators.Val._
import typelevel.combinators.Nat._
import typelevel.combinators.ReplicateM.Aux

class TypeLevelReplicateMCombinatorTest extends TypeLevelCombinatorSuite {
  def replicateM[F[_], A](implicit F: Val[F[A]]): Aux[_3, F[A :: A :: A :: HNil]] =
    ReplicateM.apply[_3]

  test("ReplicateM - Nat._3 -> Option[String] -> String :: String :: String :: HNil") {
    assertResult(show(replicateM[Option, String], "ReplicateM")) {
      "TypeTag[[Nat._3,Option[String :: String :: String :: HNil]]]"
    }
  }
}
