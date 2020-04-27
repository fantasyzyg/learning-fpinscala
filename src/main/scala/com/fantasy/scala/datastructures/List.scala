package com.fantasy.scala.datastructures


/**
 *
 * 纯函数式语言 函数式数据结构被定义为不可变
 *
 * 但是这意味着要对数据做很多额外的复制吗? 答案是否定的,既然数据是不可变的,那么我们可以直接复用它
 * 共享不可变数据可以让函数实现更高的效率。
 * 不用悲观地复制一份数据以避免对其修改或污染。
 *
 */

sealed trait List[+A]

// Nothing 是任何类的子类
case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]


// 伴生对象，与数据类型同名的单例
object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  // 可变参数  variadic function
  def apply[A](as: A*): List[A] =
    if (as.isEmpty)
      Nil
    else
      Cons(as.head, apply(as.tail: _*))

  def fill[A](n: Int, a: A): List[A] = if (n <= 0) Nil else Cons(a, fill(n - 1, a))

  /**
   * exe 3.2
   *
   */
  def tail[A](ts: List[A]): List[A] = ts match {
    case Nil => throw new NoSuchMethodException("Nil.tail")
    case Cons(_, xs) => xs
  }

  /**
   * exe 3.3
   */
  def setHead[A](a: A, xs: List[A]): List[A] = xs match {
    case Nil => throw new NoSuchMethodException("Nil.setHead")
    case Cons(_, xss) => Cons(a, xss)
  }


  /**
   * exe 3.4
   */
  @scala.annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0)
      l
    else {
      l match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }
    }
  }

  /**
   * exe 3.5
   */
  @scala.annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) =>
      if (f(x))
        dropWhile(xs, f)
      else
        l
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(x, xs) => Cons(x, append(xs, a2))
  }

  /**
   * exe 3.6
   * 因为单向链表的结构，任何时间我们想要替代Cons的尾部，甚至是链表的最后一个Cons，我们必须
   * 复制之前所有的Cons对象。
   */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new NoSuchMethodException("Nil.init")
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  val xs: List[Int] = List(1, 2, 3, 4)
  val ex1: List[Int] = dropWhile[Int](xs, x => x < 4)


  @scala.annotation.tailrec
  def dropWhile2[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Cons(h, t) if f(h) => dropWhile2(t)(f)
    case _ => as
  }

  // 我们常通过参数分组排序成多个参数列表,来最大化地利用类型推导
  val ex2: List[Int] = dropWhile2(xs)(x => x < 4)

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]): Int = foldRight(ns, 0)(_ + _)

  def product2(ns: List[Double]): Double = foldRight(ns, 1.0)(_ * _)

  /**
   * Exercise 3.7
   *
   * 不会提前终止，因为 foldRight 先遍历所有元素，将其展开，然后才真正执行 f；若要支持 early termination，需要
   * 使用非严格求值，在 Chapter 5 介绍。
   */


  /**
   * exe 3.8
   * 结果就是List(1,2,3,4), 只是简单复制一份
   */


  /**
   * exe 3.9
   */
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, b) => b + 1)


  /**
   * exe 3.10
   */
  @scala.annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }


  /**
   * exe 3.11
   */
  def sum3(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]): Double = foldLeft(ns, 1.0)(_ * _)


  /**
   * exe 3.12
   *
   * Nil : List[A] 注明类型是必要的，否则scala将推断 B 参数类型是 List[Nothing]
   */
  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((a, b) => Cons(b, a))


  /**
   * exe 3.13
   * 1. 使用 reverse + foldLeft 实现严格求值的 foldRight，以避免 foldRight 栈溢出，是一种常见技巧；
   */
  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((b, a) => f(a, b))

  def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = foldRight(reverse(as), z)((a, b) => f(b, a))

  /**
   * exe 3.14
   */

  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((a, b) => Cons(a, b))

  def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] = foldLeft(reverse(a1), a2)((b, a) => Cons(a, b))

  /**
   * exe 3.15
   */
  def concat[A](xss: List[List[A]]): List[A] = foldLeft(xss, Nil: List[A])((b, a) => append(b, a))

  /**
   * exe 3.16
   */
  def addOne(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case Cons(x, y) => Cons(x + 1, addOne(y))
  }

  def addOne2(xs: List[Int]): List[Int] = foldRight(xs, Nil: List[Int])((a, b) => Cons(a + 1, b))

  /**
   * exe 3.17
   */
  def double2String(xs: List[Double]): List[String] = xs match {
    case Nil => Nil
    case Cons(x, y) => Cons(x.toString, double2String(y))
  }

  def double2String2(xs: List[Double]): List[String] = foldRight(xs, Nil: List[String])((a, b) => Cons(a.toString, b))

  /**
   * exe 3.18
   */
  def map[A, B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => Nil
    case Cons(x, y) => Cons(f(x), map(y)(f))
  }


  def map3[A, B](xs: List[A])(f: A ⇒ B): List[B] = {
    var buffer = new scala.collection.mutable.ListBuffer[B]

    @scala.annotation.tailrec
    def aux(xs: List[A]): Unit = xs match {
      case Nil ⇒ ()
      case Cons(h, t) ⇒ buffer += f(h); aux(t)
    }

    aux(xs)

    List(buffer.toList: _*) // converting from the standard Scala list to the list we've defined here
  }

  /**
   * exe 3.19
   */
  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x, y) if f(x) => Cons(x, filter(y)(f))
    case Cons(_, y) => filter(y)(f)
  }

  /**
   * exe 3.20
   */
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

  /**
   * exe 3.21
   * 利用flatMap实现filter
   */
  def filter2[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(x => if (f(x)) List(x) else Nil)

  /**
   * exe 3.22
   */
  def addPairWise(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
    case (Cons(a, as1), Cons(b, bs1)) => Cons(a + b, addPairWise(as1, bs1))
    case _ => Nil
  }

  /**
   * exe 3.23
   */
  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Cons(a, as1), Cons(b, bs1)) => Cons(f(a, b), zipWith(as1, bs1)(f))
    case _ => Nil
  }


  /**
   * exe 3.24
   */
  // 定义辅助函数
  @scala.annotation.tailrec
  def startWith[A](xs: List[A], ys: List[A]): Boolean = (xs, ys) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(x, xss), Cons(y, yss)) if x == y => startWith(xss, yss)
    case _ => false
  }

  @scala.annotation.tailrec
  def hasSubSequence[A](xs: List[A], sub: List[A]): Boolean = (xs, sub) match {
    case (_, Nil) => true
    case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => if (startWith(t1, t2)) true else hasSubSequence(t1, sub)
    case (Cons(_, t1), _) => hasSubSequence(t1, sub)
    case _ => false
  }
}