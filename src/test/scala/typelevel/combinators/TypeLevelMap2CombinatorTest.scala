package typelevel.combinators

import typelevel.combinators.->._
import typelevel.combinators.Map2.Aux

class TypeLevelMap2CombinatorTest extends TypeLevelCombinatorSuite {
  def mapped[A, B, C](implicit F: (A, B) -> C): Aux[A :: A :: HNil, B :: B :: HNil, C :: C :: HNil] =
    Map2.apply[A :: A :: HNil, B :: B :: HNil]

  test(
    "Map2 - Int :: Int :: HNil -> Option[Int] :: Option[Int] :: HNil -> ((Int, Option[Int]) -> Int) -> Int :: Int :: HNil") {
    assertResult(show(mapped[Int, Option[Int], Int], "Map2")) {
      "TypeTag[[Int :: Int :: HNil,Option[Int] :: Option[Int] :: HNil,Int :: Int :: HNil]]"
    }
  }
}
