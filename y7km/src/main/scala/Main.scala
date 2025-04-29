import ast._

object Main {

  def main(args: Array[String]): Unit = {
    val x = Id()
    val const = Const[Double](5.0)
    val square = Mul(x, x)

    val f = Add(square, const)
    val df = d(f) // 'f

    val value = df.eval((3.0, 1.0)) // (x, dx) = (3, 1) == (x값, 변화량)
    println(s"f'(3) = $value")      // expected: 6.0
  }

  def d[A](f: Expr[A]): Expr[(A, A)] = f match {
    case Const(_)      => Const[(A, A)](0.0)
    case Id()          => Proj2[A]()
    case Add(f1, f2)   => Add(d(f1), d(f2))
    case Mul(f1, f2)   =>
      Add(
        Mul(d(f1), Compose(f2, Proj1[A]())), // df1(x, dx) * g(x)
        Mul(Compose(f1, Proj1[A]()), d(f2))  // f(x) * dg(x, dx)
      )
  }
}
