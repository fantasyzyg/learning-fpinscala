package com.fantasy.scala.gettingstarted

/**
 * Author: zhuyaogai
 *
 * 匿名函数   FunctionX 对象  apply 方法  语法糖
 *
 *  pure function   无副作用
 *  referential transparency  引用透明
 *  substitution model 替代模型，可以被推理 
 */
object Solution {

  /**
   * exe 2.1
   */
  def fib(n: Int): Int = {
    // 尾递归
    @scala.annotation.tailrec
    def go(i: Int, pre: Int, cur: Int): Int = {
      if (i == n)
        pre
      else
        go(i + 1, cur, pre + cur)
    }

    go(0, 0, 1)
  }

  /**
   * high order function
   */

  def formatResult(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  /**
   * 单态的(monomorphic)
   * 多态函数 -> 泛型函数
   */
  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @scala.annotation.tailrec
    def loop(n: Int): Int = {
      if (n >= as.length)
        -1
      else if (p(as(n)))
        n
      else
        loop(n + 1)
    }

    loop(0)
  }

  /**
   * exe 2.2
   * Array[A] 是否按照给定的 ordered 比较函数来进行排序
   */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @scala.annotation.tailrec
    def loop(cur: Int, next: Int): Boolean = {
      if (next >= as.length)
        true
      else if (ordered(as(cur), as(next)))
        loop(next, next + 1)
      else
        false
    }

    loop(0, 1)
  }

  /**
   * partial functions
   */
  def partiall[A, B, C](a: A, f: (A, B) => C): B => C = {
    b: B => f(a, b)
  }

  /**
   * exe 2.3   科里化
   */
  def curry[A, B, C](f: (A, B) => C): A => B => C = {
    a: A => {
      b: B =>
        f(a, b)
    }
  }


  /**
   * exe 2.4
   */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  /**
   * exe 2.5
   */
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a: A => f(g(a))
  }

//  def compose[A, B, C](f: B => C, g: A => B): A => C = {
//    f compose g
//  }
}
