package homework2

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalatest.FlatSpec

class PermutationsTest extends FlatSpec {

  def permList(l: List[Int]): List[List[Int]] = l match {

    case Nil => List(Nil)

    case List(ele) =>

      List(List(ele))

    case xs =>

      xs.indices

        .flatMap(i => permList(xs.slice(0, i) ++ xs.slice(i + 1, xs.length))

          .map(p => xs(i) :: p)

        ).toList

  }

  val genIntList: Gen[List[Int]] =  Gen.sized { _ => Gen.nonEmptyContainerOf[List, Int](Gen.choose(0, 20)) }

  val permutationTest = forAll(genIntList) { (list) =>
    permList(list.distinct) == list.distinct.permutations.toList
  }

  permutationTest.check
}
