package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_,t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, t) => Cons(h,t)
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else
      l match {
      case Nil => Nil
      case Cons(_,t) => drop(t,n-1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t) if f(h) => dropWhile(t,f)
      case Cons(_,_) => l
    }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, Cons(h2, Nil)) => Cons(h, Nil)
    case Cons(h,t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = foldRight(l,0)( (_, acc) => acc + 1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z,h))(f)
  }

  def sum3(ns: List[Int]) =
    foldRight2(ns, 0)(_ + _)

  def product3(ns: List[Double]) = foldRight2(ns, 1.0)(_ * _)

  def length2[A](l: List[A]) = foldLeft2(l,0)((acc, _) => acc+1)

  def reverse[A](l: List[A]) = foldLeft2(l, Nil: List[A])((acc,h ) => Cons(h, acc))

  def foldLeft2[A,B](l: List[A], z: B)(f: (B,A) => B): B = foldRight(l,z)((h, acc) => f(acc,h))

  def foldRight2[A,B](l: List[A], z: B)(f: (A,B) => B): B = foldLeft(l,z)((acc, h) => f(h, acc))

  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(l), z)((b,a) => f(a,b))

  def append2[A](l: List[A], a: A): List[A] = foldRight(l, Cons(a, Nil))(Cons(_,_))
  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_,_))

  def flatten[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])((h, acc) => appendViaFoldRight(h, acc))

  def addOne(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(h,t) => Cons(h+1, addOne(t))
  }

  def convertToString(l: List[Double]) : List[String] = l match {
    case Nil => Nil
    case Cons(h,t) => Cons(h.toString, convertToString(t))
  }

  def head[A](l: List[A]): A = l match {
    case Nil => sys.error("head of empty list")
    case Cons(h, _) => h
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = {
    def go(rest:List[A], acc: List[B]): List[B] = rest match {
      case Nil => acc
      case Cons(h,t) => go(t, appendViaFoldRight(acc, Cons(f(h), Nil)))
    }
    go(List.tail(l), Cons(f(List.head(l)), Nil))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    def go(l: List[A], acc: List[A]): List[A] = l match {
      case Nil => acc
      case Cons(h,t) if f(h) => go(t, appendViaFoldRight(acc, Cons(h, Nil)))
      case Cons(h,t) => go(t,acc)
    }
    go(as, Nil)
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    def go(l: List[A], acc: List[B]): List[B] = l match {
      case Nil => acc
      case Cons(h, t) => go(t, List.appendViaFoldRight(acc, f(h)))
    }
    go(as, Nil)
  }

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => if (f(x)) Cons(x, Nil) else Nil)

  def addTwoLists(l: List[Int], r: List[Int]): List[Int] = {
    def go(l: List[Int], r: List[Int], acc: List[Int]): List[Int] = {
      (l,r) match {
        //case (Nil, Cons(_,_)) => acc
        //case (Cons(_,_), Nil) => acc
        //case (Nil, Nil) => acc
        case (Cons(h1, t1), Cons(h2,t2)) => go(t1, t2, appendViaFoldRight(acc, Cons(h1 + h2, Nil)))
        case _ => acc
      }
    }
    go(l, r, Nil)
  }

  def zipWith[A,B,C](l: List[A], r: List[B])(f: (A,B) => C): List[C] = {
    def go(l: List[A], r: List[B], acc: List[C]): List[C] = (l,r) match {
      case (Cons(h1,t1), Cons(h2, t2)) => go(t1,t2, appendViaFoldRight(acc, Cons(f(h1,h2), Nil)))
      case _ => acc
    }
    go(l, r, Nil)
  }

  def hasSubsequence[A](as: List[A], sub: List[A]): Boolean = {
    def go[A](as: List[A], sub: List[A]): Boolean = {
      (as, sub) match {
        //case (Nil, Cons(_, _)) => false
        case (_, Nil) => true
        case (Cons(h1,t1), Cons(h2,t2)) if h1 == h2 => go(t1,t2)
        case _ => false
      }
    }
    as match {
      case Nil => false
      case _ if go(as,sub) => true
      case Cons(h, t) => hasSubsequence(t, sub)
    }
  }
}

object Start {
  def main(args: Array[String]): Unit = {
    println(List.hasSubsequence(List(1,2,3,4), List(1,2)))
  }
}
