package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a

    val hh = deleteMin(h)
    isEmpty(hh)
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h))==m
  }

  //If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.
  property("inserting two elements") = forAll({ (a : Int, b : Int, c : Int) =>
    val h = empty
    val h2 = insert(a, h)
    val h3 = insert(b, h2)
    val h4 = insert(c, h3)

    val list = List[Int](a,b,c).sorted
    val min = list.last

    val h5 = deleteMin(h4)
    val h6 = deleteMin(h5)

    findMin(h6) == min
  })

  //If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
  property("inserting and deleting minimum") = forAll({ (a : Int) =>
    val h = insert(genInt.sample.get, insert(genInt.sample.get, insert(a, empty)))
    val hh = deleteMin(deleteMin(deleteMin(h)))
    isEmpty(hh)
  } )

  //Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.
  // (Hint: recursion and helper functions are your friends.)
  property("sorted sequence of elements") = forAll({ (ha : H) =>

    val list : List[Int] = pullsOut(ha, Nil)
    val result = isSorted(list)
    result
  } )

  def pullsOut(ha : H, output : List[Int]) : List[Int] = isEmpty(ha) match {
    case true => output.reverse
    case false => {
      val min = findMin(ha)
      val haa = deleteMin(ha)
      pullsOut(haa, min :: output)
    }
  }

  def isSorted(list : List[Int]) : Boolean = (list, list.tail).zipped.forall(_ <= _)

  //Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("Melding any two heaps") = forAll({ (a : H, b: H) =>
    val aa = insert(genInt.sample.get, a)
    val minA = findMin(aa)
    val minB = findMin(b)

    val smallest = minA >= minB match {
      case true => minB
      case false => minA
    }

    val hh = meld(aa, b)

    val allInAllMin = findMin(hh)
    allInAllMin == smallest
  } )

  lazy val genHeap: Gen[H] = for {
                                a <- arbitrary[Int]
                                h <- oneOf(value(empty), genHeap, insert(genInt.sample.get, empty))
                                heap <- insert(a, h)
                              } yield heap

  lazy val genInt: Gen[Int] = for {
                                  i <- arbitrary[Int]
                                  res <- oneOf(value(Int.MaxValue), genInt)
                                } yield res

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
