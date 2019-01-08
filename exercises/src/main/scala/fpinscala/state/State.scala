package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (value, nextRng) = rng.nextInt

    value match {
      case Int.MinValue => (Int.MaxValue, nextRng)
      case n if n < 0 => (-n,nextRng)
      case n => (n, nextRng)
    }
  }

  val doubleWithMap: Rand[Double] = map(nonNegativeInt)(_.toDouble / (Int.MaxValue.toDouble + 1))

  def double(rng: RNG): (Double, RNG) = {
    val (i, nrng) = nonNegativeInt(rng)
    i.toDouble / Int.MaxValue.toDouble -> nrng
  }


  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, nrng) = rng.nextInt
    val (d, nnrng) = double(nrng)
    i -> d -> nnrng
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (i, nrng) = rng.nextInt
    val (d, nnrng) = double(nrng)
    d -> i -> nnrng
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, nrng) = double(rng)
    val (d2, nnrng) = double(nrng)
    val (d3, nnnrng) = double(nnrng)
    (d1,d2,d3) -> nnnrng
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val tempList = scala.collection.mutable.ListBuffer.newBuilder[Int]
    var crng = rng
    for (i <- 0 to count) {
      val (v, nrng) = crng.nextInt
      tempList += i
      crng = nrng
    }
    (tempList.result().toList, crng)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      f(a,b) -> r2
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match {

    case x :: Nil =>   rng => {
      val (v, nrng) = x(rng)
      List(v) -> nrng
    }
    case x :: xs => rng => {
      val (l, r1) = sequence(xs)(rng)
      val (v, r2) = x(r1)
      (v :: l) -> r2
    }
    case Nil => unit(List())
  }

  def intsViaSequence(count: Int)(rng: RNG): (List[Int],RNG) = sequence(List.fill[Rand[Int]](count)(rng => rng.nextInt))(rng)

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (v, r1) = f(rng)
      val (v1, r2) = g(v)(r1)
      v1 -> r2 //doppelmoppel, can return g(v)(r1) directly
    }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) {
    v =>
      val mod = v % n
      if(v + (n-1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
  }

  def mapViaFlat[A,B,S](s: Rand[A])(f: A => B) : Rand[B] = flatMap(s){v => unit(f(v)) }

  def map2ViaFlat[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a =>
      map(rb) { b =>
        f(a,b)
      }
    }
}

case class State[S,+A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = {
    flatMap(a => {
      State.unit(f(a))
    })
  }


  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a=> {
      sb.map(b => {
          f(a,b)
        }
      )
    })


  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })
  }

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)



object CandyMachine extends App {
//  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
//    inputs.foldRight(State.unit[Machine, (Int,Int)]((0,0)))((input, state) => {
//      State.modify[Machine] _ compose changeMachineState
//    })
//  }


  val machine = Machine(true, 5, 0)


  def changeMachineState(input: Input)(machine: Machine): Machine = {
    (machine,input) match {
      case (Machine(_,0,_ ), _)=> machine
      case (Machine(true, candies, coins), Coin) => Machine(false, candies, coins+1)
      case (Machine(true, _, _), Turn) => machine
      case (Machine(false, _, _), Coin) => machine
      case (Machine(false, candies, coins), Turn) => Machine(true,candies-1,coins)
    }
  }

//  println(changeMachineState(Turn, changeMachineState(Coin, machine)))


}

object State {
  type Rand[A] = State[RNG, A]


  def sequence[S,A](l: List[State[S, A]]): State[S, List[A]] =
    l.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)( _ :: _ ))



  def sequencePattern[S,A](l: List[State[S,A]]) : State[S, List[A]] = {
      l match {
        case Nil => unit(List.empty[A])
        case x :: Nil => {
          State(s => {
            val (v, s1) = x.run(s)
            List(v) -> s1
          }
          )
        }
        case x :: xs => {
          val r = sequence(xs)
          State(s => {
            val (l, s1) = r.run(s)
            val (v, s2) = x.run(s1)
            (v :: l) -> s2
          })
        }
      }
  }

  def unit[S, A](a: A): State[S,A] = {
    State(s => a -> s)
  }


  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

object Main extends App {
  val time = System.currentTimeMillis()
  val rng = RNG.Simple(time)
  var r0: RNG = RNG.Simple(time)
  val (i, nRng) = RNG.double(rng)
  val l = List(RNG.randDoubleInt, RNG.randIntDouble, RNG.int)
  println(l)
  println(RNG.sequence(l))
  for (ii <- l.reverse) {
    val (v, r1) = ii(r0)
    println(v)
    r0 = r1
  }
  println(RNG.sequence(l)(rng)._1)
  println(RNG.intsViaSequence(5)(rng))
}
