package typelevel.combinators
import org.scalatest.funsuite.AnyFunSuite

import scala.reflect.runtime.universe._

trait TypeLevelCombinatorSuite extends AnyFunSuite {
  def show[T](value: T, target: String)(implicit T: TypeTag[T]): String =
    T.toString
      .replace("typelevel.combinators.", "")
      .replace(s"$target.Aux", "")
}
