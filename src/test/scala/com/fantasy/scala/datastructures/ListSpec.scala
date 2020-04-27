package com.fantasy.scala.datastructures

import com.fantasy.scala.datastructures.List._
import org.scalatest.{Matchers, WordSpec}

class ListSpec extends WordSpec with Matchers {

  "sum" should {
    "return 0 on Nil" in {
      sum(Nil) shouldEqual 0
    }
    "work fine with non empty list" in {
      sum(List(1, 2, 3, 4, -1)) shouldEqual 9
    }
  }

  "product" should {
    "return 1.0 on Nil" in {
      product(Nil) shouldEqual 1.0
    }
    "return 0 for any list contains 0" in {
      product(List(1, 2, 3.0, 0)) shouldEqual 0
    }
    "return the product of list" in {
      product(List(1, 2.5, 3)) shouldEqual 7.5
    }
  }

  "tail" should {
    "throw NoSuchMethodException on empty list" in {
      intercept[NoSuchMethodException] {
        tail(Nil)
      }
    }
    "return all but the first element of a list" in {
      tail(List(1, 2, 3)) shouldEqual List(2, 3)
    }
  }

  "setHead" should {
    "throw NoSuchMethodException on empty list" in {
      intercept[NoSuchMethodException] {
        setHead(0, Nil)
      }
    }
    "return a new list with the given head" in {
      setHead(0, List(1, 2, 3)) shouldEqual List(0, 2, 3)
    }
  }

  "drop" should {
    "return the original list if n is less or equal than 0" in {
      drop(List(1, 2, 3), 0) shouldEqual List(1, 2, 3)
      drop(List(1, 2, 3), -2) shouldEqual List(1, 2, 3)
    }
    "return Nil on empty list" in {
      drop(Nil, 10) shouldEqual Nil
    }
    "return all but the first n elements" in {
      drop(List(1, 2, 3, 4), 2) shouldEqual List(3, 4)
    }
  }

  "dropWhile" should {
    "drop the element until predicate get true" in {
      dropWhile[Int](List(1, 2, 3, 4), _ % 2 != 0) shouldEqual List(2, 3, 4)
    }
    "return Nil on empty list" in {
      dropWhile[Int](Nil, _ ⇒ true) shouldEqual Nil
    }
  }

  "dropWhile2" should {
    "drop the element until predicate get true" in {
      dropWhile2(List(1, 2, 3, 4))(_ % 2 != 0) shouldEqual List(2, 3, 4) // 第二个参数组，不需要指明类型
    }
    "return Nil on empty list" in {
      dropWhile2(Nil: List[Int])(_ => true) shouldEqual Nil
    }
  }

  "init" should {
    "throw exception on empty list" in {
      intercept[Exception] {
        init(Nil)
      }
    }
    "return all but the last element" in {
      init(List(1)) shouldEqual Nil
      init(List(1, 2, 3, 4)) shouldEqual List(1, 2, 3)
    }
  }

  "foldRight" should {
    "build sum with foldRight" in {
      def sum(l: List[Int]): Int = foldRight(l, 0)(_ + _)

      sum(List(1, 2, 3)) shouldEqual 6
      sum(Nil) shouldEqual 0
    }
    "build product with foldRight" in {
      def product(l: List[Double]): Double = foldRight(l, 1.0)(_ * _)

      product(List(1, 2, 3, 4)) shouldEqual 24.0
      product(Nil) shouldEqual 1.0
    }
  }


  "foldLeft" should {
    "build sum with foldLeft" in {
      sum2(List(1, 2, 3)) shouldEqual 6
      sum2(Nil) shouldEqual 0
    }
    "build product with foldLeft" in {
      product2(List(1, 2, 3, 4)) shouldEqual 24.0
      product2(Nil) shouldEqual 1.0
    }
  }

  "reverse" should {
    "return Nil on empty list" in {
      reverse(Nil) shouldEqual Nil
    }
    "work fine with non empty list" in {
      reverse(List("A", "B")) shouldEqual List("B", "A")
    }
  }

  "foldLeftViaFoldRight" should {
    "behave exactly like foldLeft" in {
      val xs = List(1, 2, 3, 4)
      foldLeftViaFoldRight(xs, 0)(_ + _) shouldEqual foldLeft(xs, 0)(_ + _)
      foldLeftViaFoldRight(xs, Nil: List[Int])((l, x) ⇒ Cons(x, l)) shouldEqual foldLeft(xs, List[Int]())((l, x) ⇒ Cons(x, l))
    }
  }

  "appendViaFoldRight" should {
    "work fine with empty list" in {
      appendViaFoldRight(Nil, Nil) shouldEqual Nil
      appendViaFoldRight(Nil, List(1, 2)) shouldEqual List(1, 2)
      appendViaFoldRight(List(1, 2), Nil) shouldEqual List(1, 2)
    }
    "work fine with non empty list" in {
      appendViaFoldRight(List(1, 2), List(3, 4)) shouldEqual List(1, 2, 3, 4)
    }
  }

  "appendViaFoldLeft" should {
    "work fine with empty list" in {
      appendViaFoldLeft(Nil, Nil) shouldEqual Nil
      appendViaFoldLeft(Nil, List(1, 2)) shouldEqual List(1, 2)
      appendViaFoldLeft(List(1, 2), Nil) shouldEqual List(1, 2)
    }
    "work fine with non empty list" in {
      appendViaFoldLeft(List(1, 2), List(3, 4)) shouldEqual List(1, 2, 3, 4)
    }
  }

  "concat" should {
    "return Nil on empty list" in {
      concat(Nil) shouldEqual Nil
      concat(List(Nil)) shouldEqual Nil
    }
    "flatten the give list" in {
      concat(List(List(1, 2), List("A", "B"), List('a', 'b'))) shouldEqual List(1, 2, "A", "B", 'a', 'b')
    }
  }

  "addOne" should {
    "return Nil on empty list" in {
      addOne(Nil) shouldEqual Nil
    }
    "add 1 to every element in the given list" in {
      addOne(List(1, 2, 3)) shouldEqual List(2, 3, 4)
    }
  }

  "double2String" should {
    "return Nil on empty list" in {
      double2String(Nil) shouldEqual Nil
    }
    "convert every element to String" in {
      double2String(List(1.1, 2.2)) shouldEqual List("1.1", "2.2")
    }
  }

  "filter" should {
    "return Nil on empty list" in {
      filter(Nil: List[Int])(_ % 2 == 0) shouldEqual Nil
    }
    "return a list containing all elements that confirm to f" in {
      filter(List(1, 2, 3, 4))(_ % 2 == 0) shouldEqual List(2, 4)
    }
  }

  "filter2" should {
    "return Nil on empty list" in {
      filter2(Nil: List[Int])(_ % 2 == 0) shouldEqual Nil
    }
    "return a list containing all elements that confirm to f" in {
      filter2(List(1, 2, 3, 4))(_ % 2 == 0) shouldEqual List(2, 4)
    }
  }


  "flatMap" should {
    "return Nil on empty list" in {
      flatMap(Nil: List[Int])(x ⇒ List(x, x)) shouldEqual Nil
    }
    "work fine with non empty list" in {
      flatMap(List(1, 2, 3))(x ⇒ List(x, x)) shouldEqual List(1, 1, 2, 2, 3, 3)
    }
  }


  "addPairWise" should {
    "work fine with lists of same length" in {
      addPairWise(List(1, 2, 3), List(4, 5, 6)) shouldEqual List(5, 7, 9)
    }
    "work fine with lists of different length" in {
      addPairWise(List(1, 2, 3), List(4, 5, 6, 7)) shouldEqual List(5, 7, 9)
    }
  }

  "zipWith" should {
    "work fine with lists of same length" in {
      zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _) shouldEqual List(5, 7, 9)
    }
    "work fine with lists of different length" in {
      zipWith(List(1, 2, 3), List("A", "B", "C", "D"))(_ + _) shouldEqual List("1A", "2B", "3C")
    }
  }

  "hasSubSequence" should {
    "return true if sub sequence is empty" in {
      hasSubSequence(Nil, Nil) shouldEqual true
    }
    "return true if xs has the sub sequence" in {
      hasSubSequence(List(1, 2, 3, 4), List(1, 2, 3)) shouldEqual true
      hasSubSequence(List(1, 2, 3, 4), List(1, 2, 3, 4)) shouldEqual true
    }
    "return false if xs does not have the sub sequence" in {
      hasSubSequence(List(1, 2, 3, 4), List(1, 4, 3)) shouldEqual false
      hasSubSequence(List(1, 2, 3, 4), List(1, 2, 3, 4, 5)) shouldEqual false
    }
  }

}
