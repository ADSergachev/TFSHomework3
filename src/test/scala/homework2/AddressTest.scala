package homework2

import homework2.AddressObject.Address
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.{FlatSpec, Matchers}

class AddressTest extends FlatSpec with Matchers {


  //в валидКомбос добавляем все благоприятные исходы
  val validCombos =
    Table(
      ("list", "address"),
      (List("Ленина", 12, Some(11)), Address("Ленина", 12, Some(11)))
    )

  //в инвалидКомбос добавляем все неблагоприятные исходы
  val invalidCombos =
    Table(
      "list",
      List("Ленина"),
      List()
    )

  it should "check implicit function" in {
    forAll(validCombos) { (list: List[Any], address: Address) =>
      AddressObject.listToAddress(list) shouldEqual address
    }
  }

  it should "throw IllegalArgumentException" in {
    forAll(invalidCombos) { (list: List[Any]) =>
      a[IllegalArgumentException] should be thrownBy {
        AddressObject.listToAddress(list)
      }
    }
  }
}
