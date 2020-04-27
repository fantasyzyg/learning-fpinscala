package com.fantasy.scala.errorhandling

/**
 * 完全函数 ： 对每一个输入都会有一个对应的输出函数
 */

sealed trait Option[+A] {

  /**
   * exe 4.1
   * 下面的都是针对单值的运算，后面还有针对集合的Option运算
   */
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(v) => f(v)
  }

  // default : => B 表示类型参数是 B，但不是马上求值      lazy 求值
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(v) if f(v) => this
    case _ => None
  }
}

case object None extends Option[Nothing]

case class Some[+A](get: A) extends Option[A]

object Option {
  def mean(xs: Seq[Double]): Option[Double] = xs match {
    case Nil => None
    case _ => Some(xs.sum / xs.length)
  }

  /**
   * exe 4.2
   */
  def variance(xs: Seq[Double]): Option[Double] = xs match {
    case Nil => None
    case _ => mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  // 函数得到提升 lift ，不必去进行更多的修改
  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f


  /**
   * exe 4.3
   */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(a), Some(b)) => Some(f(a, b))
    case _ => None
  }

  // 函数提升
  val abs0: Option[Int] => Option[Int] = lift(math.abs)


  // 接收一个非严格求值的A参数，我们可以对a求值是捕获任何异常并转换为None
  // 把一个基于异常的API转换为面向Option的API
  def Try[A](a: => A): Option[A] =
    try
      Some(a)
    catch {
      case _: Exception => None
    }


  def map3[A, B, C, D](a: Option[A], b: Option[B], c: Option[C])(f: (A, B, C) => D): Option[D] = (a, b, c) match {
    case (Some(a), Some(b), Some(c)) => Some(f(a, b, c))
    case _ => None
  }


  /**
   * exe 4.4
   * 思路考虑到 foldRight，但是不知道 Option[A] 和 Option[List[A]]，应该使用函数提升
   */
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight(Some(Nil): Option[List[A]])((h, t) => map2(h, t)(_ :: _))


  def parseInts(a: List[String]): Option[List[Int]] =
    sequence(a map (i => Try(i.toInt)))


  /**
   * exe 4.5
   */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(Nil): Option[List[B]])((h, t) => map2(f(h), t)(_ :: _))
}

