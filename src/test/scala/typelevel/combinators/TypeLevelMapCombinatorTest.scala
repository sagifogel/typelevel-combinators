package typelevel.combinators

import typelevel.combinators.Map.{Aux, _}
import typelevel.combinators.->._

class TypeLevelMapCombinatorTest extends TypeLevelSuite {
  def mapped[A, B](implicit ev: A -> B): Aux[A :: A :: HNil, B :: B :: HNil] = Map.apply[A :: A :: HNil]

  test("Map [Int :: Int :: HNil] -> (A -> B): [String :: String :: HNil]") {
    assertResult(show(mapped[Int, String], "Map")) {
      "TypeTag[[Int :: Int :: HNil,String :: String :: HNil]]"
    }
  }
}