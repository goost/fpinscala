package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def toList(): List[A] = foldRight[List[A]](Nil){(x, a) => x :: a}

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = if (n == 0) empty else this match {
    case b @ Empty => b
    case Cons(h, t) => cons(h(), t().take(n-1))
  }

  def drop(n: Int): Stream[A] = this match {
    case b @ Empty => b
    case Cons(_, t) if n != 0 => t().drop(n-1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
    case _ => empty
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a,b) => p(a) && b)

  def takeWhile2(p: A => Boolean) : Stream[A] =
    foldRight(empty[A]){ (a,b) =>
      if(p(a)) cons(a, b)
      else empty
    }

  def takeWhile3(p: A=> Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  def zipWith[B,C](l: Stream[B])(f: (A,B) => C): Stream[C] = unfold((this, l)) {
    case (Cons(h,t), Cons(h2,t2)) => Some(f(h(),h2()), (t(), t2()))
    case _ => None
  }

  def zipAll[B](l: Stream[B]):Stream[(Option[A], Option[B])] = unfold((this, l)) {
    case (Cons(h,t), Cons(h2,t2)) => Some((Some(h()),Some(h2())), (t(),t2()))
    case (Cons(h,t), Empty) => Some((Some(h()), None), (t(), empty))
    case (Empty, Cons(h,t)) => Some( (None, Some(h())), (empty, t()) )
    case (Empty, Empty) => None
  }
  def headOption: Option[A] = foldRight(None: Option[A]){ (a, _) =>
    Some(a)
  }

  def map[B](f: A=> B): Stream[B] = foldRight(empty[B]) {(a,b) =>
    cons(f(a), b)
  }

  def map2[B](f: A=>B): Stream[B] = unfold(this){s => s.headOption match {
    case None => None
    case Some(h) => Some(f(h), s.drop(1))
  }}

  def take2(n: Int):Stream[A] = unfold((this,n)){
    case (Cons(h,t),z) if z > 0 => Some((h(), (t(),z-1)))
    case _ => None
  }

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((a,b) =>
    if(p(a)) cons(a,b)
    else b
  )

  def append[B >: A](s: => Stream[B]):Stream[B] = foldRight(s)(cons(_,_))

  def flatMap[B](f: A=> Stream[B]): Stream[B] = foldRight(empty[B]){(a,b) =>
   f(a).append(b)
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWithFaulty[B](s: Stream[B]): Boolean = !zipWith(s) {_ == _}.exists(!_)

  def startsWith[B] (s: Stream[B]): Boolean = zipAll(s).takeWhile3(_._2.isDefined).forAll {
    case (h,h2) => h == h2
  }

  def tailsComplicated: Stream[Stream[A]] = unfold((this,false)) {
    case (Empty, true) => None
    case (Empty, false) => Some(empty, (empty, true))
    case (c @ Cons(_,t),_) => Some(unfold(c: Stream[A]){
      case Empty => None
      case Cons(hinner,tinner) => Some(hinner(), tinner())
    } -> (t(),false))
  }

  def tails: Stream[Stream[A]] = unfold(this) {
    case Empty => None
    case c @ Cons(_, t) => Some(c, t())
  } append Stream(empty)

  def scanRightDoubleSpace[B](z: =>B)(f: (A, => B) => B): Stream[B] = {
    unfold(this) {
      case Empty => None
      case s => Some( (s.foldRight(z)(f), s drop 1 ))
    } append Stream(z)
  }

  def scanRight[B](z: => B)(f: (A, => B) =>B): Stream[B] =
    foldRight((z, Stream(z))){ (v, s) =>
      lazy val es = s
      val nv = f(v, es._1)
      (nv, cons(nv, es._2))
    }._2

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  def fibs(): Stream[Int] = {
    def generate(current: Int, previous: Int): Stream[Int] = {
      cons(current, generate(current + previous, current))
    }
    generate(0,1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
        case None => empty
        case Some((v, n)) => cons(v, unfold(n)(f))
      }
    }

  val ones2: Stream[Int] = unfold(1)(_ => Some((1,1)))

  def constant2[A](a: A): Stream[A] = unfold(a)(_ => Some(a,a))

  def from2(n: Int): Stream[Int] = unfold(n)(x => Some(x, x+1))

  def fibs2(): Stream[Int] = unfold((0,1))(s => Some(s._1,(s._2, s._1 + s._2)))

}

object Main extends App {
  val s = Stream(1,2,3,4,5,6,7,8,9)
  val s2 = Stream(10,11,12)
  //println(s2.drop(1).drop(1).headOption)
 println(Stream(1,2,3).scanRightDoubleSpace(0)(_ + _).toList())
}