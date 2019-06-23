package fpinscala.parallelism

import java.util.concurrent._
import language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]
  
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = _ => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true 
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false 
    def cancel(evenIfRunning: Boolean): Boolean = false 
  }
//
//  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
//    (es: ExecutorService) => {
//      val af = a(es)
//      val bf = b(es)
//      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
//    }

  def map2[A,B,C](a: Par[A], b:Par[B])(f: (A,B) => C): Par[C] = (es: ExecutorService) =>  CombinedFuture(a(es), b(es))(f)

  def map4[A,B,C,D,E,F](a: Par[A],b: Par[B],c: Par[C],d: Par[D], e: Par[E])(f: (A,B,C,D,E) => F): Par[F] = {
    val t = map2(a,b)((_,_))
    val t2 = map2(c,d)((_,_))
    val t3 = map2(t2,e)((_,_))
    map2(t,t3)((p1,p2) => f(p1._1,p1._2,p2._1._1,p2._1._2,p2._2))
  }

  def map4[A,B,C,D,E](a: Par[A],b: Par[B],c: Par[C],d: Par[D])(f: (A,B,C,D) => E): Par[E] = {
    map2(map2(a,b)((_,_)),map2(c,d)((_,_)))((p1,p2) => f(p1._1,p1._2,p2._1,p2._2))
  }

  def map3[A,B,C,D](a: Par[A],b: Par[B],c: Par[C])(f: (A,B,C) => D): Par[D] = {
    map2(map2(a,b)((_,_)),map2(c,unit())((_,_)))((p1,p2) => f(p1._1,p1._2,p2._1))
  }

  private case class CombinedFuture[A,B,C](l: Future[A], r: Future[B])(f:(A,B) => C) extends Future[C] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = l.cancel(mayInterruptIfRunning) && r.cancel(mayInterruptIfRunning)

    override def isCancelled: Boolean = l.isCancelled && r.isCancelled

    override def isDone: Boolean = l.isDone && r.isDone

    override def get(): C = f(l.get, r.get)

    override def get(timeout: Long, unit: TimeUnit): C = {
      val deadline = System.nanoTime() + unit.toNanos(timeout)
      val lr = l.get(timeout, unit)
      val remaining = deadline - System.nanoTime()
      val rr = r.get(remaining, TimeUnit.NANOSECONDS)
      f(lr,rr)
    }
  }

  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))
  
  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] { 
      def call = a(es).get
    })

  def map[A,B](pa: Par[A])(f: A => B): Par[B] = 
    map2(pa, unit(()))((a,_) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def sequence[A](ps: List[Par[A]]) : Par[List[A]] = {
    ps.foldRight(unit(List.empty[A]))( (v, a) => map2(v, a)(_ :: _))
  }

  def parMap[A,B](ps: List[A])(f: A=>B): Par[List[B]] = fork {
    val fbs = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val u = (a: A) => (f(a), a)
    val fbs = parMap(as)(u)
    map(fbs)(_.filter(_._1).map(_._2))
  }

  def parFold[A](z: => A)(f: (A,A) => A)(as: IndexedSeq[A]): Par[A] = fork {
    if (as.size <= 1)
      unit(as.headOption getOrElse z)
    else {
      val (l,r) = as.splitAt(as.length/2)
      map2(parFold(z)(f)(l), parFold(z)(f)(r))(f)
    }
  }

  def parFold2[A,B](z: => B)(t: A => B)(f: (B,B) => B)(as: IndexedSeq[A]): Par[B] = fork {
    if (as.size <= 1)
      unit(as.headOption.fold(z)(t))
    else {
      val (l,r) = as.splitAt(as.length/2)
      map2(parFold2(z)(t)(f)(l), parFold2(z)(t)(f)(r))(f)
    }
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = 
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] = 
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => 
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }
}

object Examples extends  App{
  import Par._
//  def sum(ints: IndexedSeq[Int]): Par[Int] = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
//    if (ints.size <= 1)
//      Par.unit(ints.headOption getOrElse 0) // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
//    else {
//      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
//      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_+_)  // Recursively sum both halves and add the results together.
//    }
  val sum = Par.parFold(0)(_ + _)_
  val max = Par.parFold(Int.MinValue)((a,b) => if (a >= b) a else b)_
  val words = Par.parFold2(0)((p: String) => p.split(" ").length)(_ + _)_

  val ex = Executors.newFixedThreadPool(1)//Executors.newCachedThreadPool(Executors.defaultThreadFactory())
  val start = System.nanoTime()
  println(fork(fork(unit(2)))(ex).get)
  //println(max(IndexedSeq(1,22,3,4))(ex).get())
  //println(words(IndexedSeq("Ich esse gerne Kuchen", "Nachts, unterm Mondschein", "ja oh ja!"))(ex).get)

  //println(parFilter(List(1,2,3,4))(_%2 == 0)(ex).get)
  ex.shutdown()

}
