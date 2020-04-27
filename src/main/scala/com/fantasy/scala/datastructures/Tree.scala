package com.fantasy.scala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  /**
   * exe 3.25
   */
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + size(right) + 1
  }

  /**
   * exe 3.26
   */
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(value) => value
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  /**
   * exe 3.27
   */
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(left, right) => (depth(left) max depth(right)) + 1
  }

  /**
   * exe 3.28
   */
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  /**
   * exe 3.29
   */
  def fold[A, B](tree: Tree[A])(f: A => B)(combine: (B, B) => B): B = tree match {
    case Leaf(value) => f(value)
    case Branch(left, right) => combine(fold(left)(f)(combine), fold(right)(f)(combine))
  }


  def sizeViaFold[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(_ + _ + 1)

  def maximumViaFold(tree: Tree[Int]): Int = fold(tree)(x => x)(math.max)

  def depthViaFold[A](tree: Tree[A]): Int = fold(tree)(_ => 0)((a, b) => (a max b) + 1)

  def mapViaFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    def leaf[A1](v: A1): Tree[A1] = Leaf(v)

    def branch[A2](l: Tree[A2], r: Tree[A2]): Tree[A2] = Branch(l, r)

    fold(tree)(x => leaf(f(x)))(branch)
  }
}
