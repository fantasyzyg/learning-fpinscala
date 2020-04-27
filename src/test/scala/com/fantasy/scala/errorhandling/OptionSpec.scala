package com.fantasy.scala.errorhandling

import org.scalatest.{Matchers, WordSpec}
import com.fantasy.scala.errorhandling.Option._
import com.fantasy.scala.errorhandling._

class OptionSpec extends WordSpec with Matchers {


  "map" should {
    "work fine with None" in {
      None map (_: Int ⇒ _) shouldEqual None
    }
    "work fine with Some" in {
      Some(10) map (_ * 2) shouldEqual Some(20)
    }
  }

  "flatMap" should {
    val f = (x: Int) ⇒ if (x > 0) Some(x) else None

    "work fine with None" in {
      None flatMap f shouldEqual None
    }
    "work fine with Some" in {
      Some(10) flatMap f shouldEqual Some(10)
      Some(-10) flatMap f shouldEqual None
    }
  }


  "getOrElse" should {
    "work fine with None" in {
      None getOrElse 10 shouldEqual 10
    }
    "work fine with Some" in {
      Some(10) getOrElse -10 shouldEqual 10
    }
  }

  "orElse" should {
    "work fine with None" in {
      None orElse Some(10) shouldEqual Some(10)
    }
    "work fine with Some" in {
      Some(10) orElse Some(-10) shouldEqual Some(10)
    }
  }

  "filter" should {
    "work fine with None" in {
      None filter (_ ⇒ true) shouldEqual None
    }
    "work fine with Some" in {
      Some(10) filter (_ % 2 == 0) shouldEqual Some(10)
      Some(10) filter (_ % 2 != 0) shouldEqual None
    }
  }

  "variance" should {
    "return None on empty list" in {
      variance(Nil) shouldEqual None
    }
    "return the variance on non empty list" in {
      variance(Seq(10, 10)) shouldEqual Some(0)
    }
  }

  "map2" should {
    "be able to convert parameters to Option" in {
      map2(Some("A") , Some(101))(_ + _) shouldEqual Some("A101")
    }
  }

  "sequence" should {
    "return Some(Nil) if the given Option list contains None" in {
      sequence(None :: Nil) shouldEqual None
      sequence(List(Some("A"), Some("B"), Some(101), None)) shouldEqual None
    }
    "convert List[Option] to Option[List]" in {
      sequence(List(Some("A"), Some("B"), Some(101))) shouldEqual Some(List("A", "B", 101))
    }
  }

  "traverse" should {
    "work fine with empty list" in {
      traverse(Nil)(Some(_)) shouldEqual Some(Nil)
    }
    "work fine on non empty list" in {
      traverse(List(1, 2, 3, 4))(Some(_)) shouldEqual Some(List(1, 2, 3, 4))
    }
    "return None if one element get None after applied f" in {
      traverse(List(1, 2, 3, 4))(x ⇒ if (x % 2 == 0) Some(x) else None) shouldEqual None
    }
  }


}