package typelevel.combinators

import cats.Applicative
import typelevel.combinators.Traverse.Aux

class TypeLevelTraverseCombinatorTest extends TypeLevelCombinatorSuite {
  def traversed[F[_]: Applicative, A, B](implicit ev: A -> F[B]): Aux[A :: A :: HNil, F[B :: B :: HNil]] =
    Traverse.apply[A :: A :: HNil]

  test("Traverse [Int :: Int :: HNil] -> (Int -> Option[String]) -> Option[String :: String :: HNil]]") {
    assertResult(show(traversed[Option, Int, String], "Traverse")) {
      "TypeTag[[Int :: Int :: HNil,Option[String :: String :: HNil]]]"
    }
  }
}
