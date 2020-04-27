package com.fantasy.scala.errorhandling

/**
 * Option 不会告诉我们在异常条件下发生了什么错误，它只是给我们一个None，表示没有可用的值
 * 但是有时候我们想知道得更多
 */

// E -> Error   A -> Answer
trait Either[+E, +A] {
  /**
   * exe 4.6
   */
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(v) => Left(v)
    case Right(v) => Right(f(v))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(v) => Left(v)
    case Right(v) => f(v)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(v) => Right(v)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
    case (Right(v1), Right(v2)) => Right(f(v1, v2))
    case (Left(v), _) => Left(v)
    case (_, Left(v)) => Left(v)
  }

  def map21[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this flatMap (aa => b map (bb => f(aa, bb)))

}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

// 就像Option，Either只有两种情况，但是Either两种情况都会有值


sealed class Name(val value: String)

sealed class Age(val value: Int)

case class Person(name: Name, age: Age)

object Either {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)


  // Try函数的目的就是把异常转变为我们想要的形式
  def Try[A](a: => A): Either[Exception, A] =
    try
      Right(a)
    catch {
      case e: Exception => Left(e)
    }

  def safeDiv(x: Int, y: Int): Either[Exception, Double] =
    Try(x / y)

  /**
   * exe 4.7
   */
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es.foldRight(Right(Nil): Either[E, List[A]])((h, t) => h.map2(t)(_ :: _))


  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight(Right(Nil): Either[E, List[B]])((h, t) => f(h).map2(t)(_ :: _))
}