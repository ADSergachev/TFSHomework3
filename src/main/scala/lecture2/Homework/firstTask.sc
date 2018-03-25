def permutate(l: List[Int]): List[List[Int]] = l match {
  case List(pl) => List(List(pl))
  case pList =>
    for {
      i <- List.range(0, pList.length)
      p <- permutate(pList.slice(0, i) ++ pList.slice(i + 1, pList.length))
    } yield pList(i) :: p
}