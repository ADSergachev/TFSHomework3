case class Person(name: String, age: Int, phone: String)

def mapToList(m: Map[Int, Tuple3[String, Int, String]]): List[Person] = {
  m
    .map(tuple => Person(tuple._2._1, tuple._2._2, tuple._2._3))
    .toList
}

