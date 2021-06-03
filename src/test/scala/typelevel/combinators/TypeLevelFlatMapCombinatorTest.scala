package typelevel.combinators

import typelevel.combinators.FlatMap.{Aux, _}
import typelevel.combinators.->._

class TypeLevelFlatMapCombinatorTest extends TypeLevelCombinatorSuite {
  def flatMapped[A, B](implicit F: A -> (B :: HList)): Aux[A :: A :: HNil, B :: B :: HNil] =
    FlatMap.apply[A :: A :: HNil]

  test("FlatMap - Int :: Int :: HNil -> (Int -> String :: HList) -> String :: String :: HNil") {
    assertResult(show(flatMapped[Int, String], "FlatMap")) {
      "TypeTag[[Int :: Int :: HNil,String :: String :: HNil]]"
    }
  }
}
