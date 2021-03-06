package com.fantasy.scala.datastructures

import org.scalatest.{Matchers, WordSpec}


class TreeSpec extends WordSpec with Matchers {

  import com.fantasy.scala.datastructures.Tree._
  "size" should {
    "return sum of leaf and branch" in {
      Tree.size(Branch(Leaf(1), Leaf(2))) shouldEqual 3
      Tree.size(Leaf(2)) shouldEqual 1
    }
  }

  "maximum" should {
    "return the max element in the tree" in {
      maximum(Branch(Leaf(1), Leaf(2))) shouldEqual 2
      maximum(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(40), Leaf(0)))) shouldEqual 40
      maximum(Branch(Branch(Leaf(1), Leaf(2)), Leaf(40))) shouldEqual 40
    }
  }

  "depth" should {
    "return the max depth of a tree" in {
      depth(Branch(Leaf(1), Leaf(2))) shouldEqual 1
      depth(Branch(Branch(Leaf(1), Leaf(2)), Leaf(40))) shouldEqual 2
    }
  }

  "map" should {
    "apply f to all element" in {
      map(Leaf(1))(_ ⇒ "A") shouldEqual Leaf("A")
      map(Branch(Branch(Leaf(1), Leaf(2)), Leaf(40)))(_ + 1) shouldEqual Branch(Branch(Leaf(2), Leaf(3)), Leaf(41))
    }
  }

  "fold" should {
    "be able to form size" in {
      sizeViaFold(Branch(Leaf(1), Leaf(2))) shouldEqual 3
      sizeViaFold(Leaf(2)) shouldEqual 1
    }

    "be able to form maximum" in {
      maximumViaFold(Branch(Leaf(1), Leaf(2))) shouldEqual 2
      maximumViaFold(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(40), Leaf(0)))) shouldEqual 40
      maximumViaFold(Branch(Branch(Leaf(1), Leaf(2)), Leaf(40))) shouldEqual 40
    }

    "be able to form depth" in {
      depthViaFold(Branch(Leaf(1), Leaf(2))) shouldEqual 1
      depthViaFold(Branch(Branch(Leaf(1), Leaf(2)), Leaf(40))) shouldEqual 2
    }

    "be able to form map" in {
      mapViaFold(Leaf(1))(_ ⇒ "A") shouldEqual Leaf("A")
      mapViaFold(Branch(Branch(Leaf(1), Leaf(2)), Leaf(40)))(_ + 1) shouldEqual Branch(Branch(Leaf(2), Leaf(3)), Leaf(41))
    }
  }

}
