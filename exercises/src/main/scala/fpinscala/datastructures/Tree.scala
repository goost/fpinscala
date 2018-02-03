package fpinscala.datastructures

import fpinscala.datastructures.Tree.{maximum, size}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = {
    t match {
      case Leaf(x) => x
      case Branch(l,r) => maximum(l) max maximum(r)
    }
  }

  def depth[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }
  }

  def map[A,B](t: Tree[A])(f: A => B) : Tree[B] = {
    t match {
      case Leaf(x) => Leaf(f(x))
      case Branch(l, r) => Branch(map(l)(f),map(r)(f))
    }
  }

  def fold[A,B](t: Tree[A], zf: A => B)(f: (B, B) => B): B = t match {
    case Leaf(x) => zf(x)
    case Branch(l, r) =>  f(fold(l,zf)(f), fold(r,zf)(f))
  }

  def size2[A](t:Tree[A]): Int = fold(t, (_:A) => 1)(1 + _+ _)

  def maximum2(t:Tree[Int]): Int = fold(t, (x: Int) => x)(_ max _)

  def depth2[A](t: Tree[A]): Int = fold(t, (x:A) => 1)( (x,y) => (x max y) + 1)

  def map2[A,B](t:Tree[A])(f: A=> B): Tree[B] = fold (t, (x:A) => Leaf(f(x)): Tree[B])((x,y) => Branch(x,y))
}


object Main extends App {

    val tree = Branch(
      Branch(
        Leaf(1),
        Leaf(15)),
      Branch(
        Branch(
          Branch(
            Leaf(4),
            Leaf(18)
          ),
          Leaf(22)),
        Leaf(10))
    )
    println(Tree.size(tree))
    println(Tree.size2(tree))
    println(Tree.maximum(tree))
    println(Tree.maximum2(tree))
  println(Tree.depth(tree))
  println(Tree.depth2(tree))
  println(Tree.map(tree)(_ * 2))
  println(Tree.map2(tree)(_ * 2))

}