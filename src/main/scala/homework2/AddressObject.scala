package homework2

object AddressObject {
  case class Address(street: String, house: Int, apartment: Option[Int]) {
    override def toString: String = s"Address street: ${street}, house: ${house}, apartment: ${apartment.getOrElse("not defined")}."
  }

  implicit def listToAddress(xs: List[Any]): Address = {
    xs match {
      case List(street: String, house: Int, apartment: Int, _*) => Address(street, house, Some(apartment))
      case List(street: String, house: Int, apartment: Some[Int], _*) => Address(street, house, apartment)
      case List(street: String, house: Int, _*) => Address(street, house, None)
      case _ => throw new IllegalArgumentException("Не удалось создать объект типа Address, неверные параметры конструктора")
    }
  }

  def printAddress(address: Address): Unit = {
    println(address)
  }

}