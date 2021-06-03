package typelevel.combinators

import typelevel.combinators.->._
import typelevel.combinators.Product.Aux

class TypeLevelProductCombinatorTest extends TypeLevelCombinatorSuite {
  def product[A, B]: Aux[A :: A :: HNil, B :: B :: HNil, (A, B) :: (A, B) :: HNil] =
    Product.apply[A :: A :: HNil, B :: B :: HNil]

  test("Product - Int :: Int :: HNil -> String :: String :: HNil -> (String, Int)  :: (String, Int)  :: HNil") {
    assertResult(show(product[String, Int], "Product")) {
      "TypeTag[[String :: String :: HNil,Int :: Int :: HNil,(String, Int) :: (String, Int) :: HNil]]"
    }
  }
}
