import ast._
import cats.effect.{IO, IOApp}

object Main extends IOApp.Simple {

  def defineFunction: IO[Expr[Double]] = IO {
    val x = Id()
    val const = Const[Double](5.0)
    val square = Mul(x, x)
    Add(square, const) // f(x) = x^3 + 5
  }

  def derive(f: Expr[Double]): IO[Expr[(Double, Double)]] = IO {
    d(f)
  }

  def printResults(f: Expr[Double], df: Expr[(Double, Double)]): IO[Unit] = for {
    _ <- IO.println(s"f(x)   = ${prettyPrint(f)}")
    _ <- IO.println(s"f'(x)  = ${prettyPrint(df)}")
    result = df.eval((3.0, 1.0)) // (x, dx)
    _ <- IO.println(s"f'(3)  = $result")
  } yield ()

  override def run: IO[Unit] = for {
    f  <- defineFunction
    df <- derive(f)
    _  <- printResults(f, df)
  } yield ()

  def d[A](f: Expr[A]): Expr[(A, A)] = f match {
    case Const(_)    => Const[(A, A)](0.0)
    case Id()        => Proj2[A]()
    case Add(f1, f2) => Add(d(f1), d(f2))
    case Mul(f1, f2) =>
      Add(
        Mul(d(f1), Compose(f2, Proj1[A]())),
        Mul(Compose(f1, Proj1[A]()), d(f2))
      )
  }

  def prettyPrint[A](expr: Expr[A]): String = expr match {
    case Const(v)      => v.toString
    case Id()          => "x"
    case Proj1()       => "x"
    case Proj2()       => "dx"
    case Add(l, r)     => s"(${prettyPrint(l)} + ${prettyPrint(r)})"
    case Mul(l, r)     => s"(${prettyPrint(l)} * ${prettyPrint(r)})"
    case Compose(f, g) => s"${prettyPrint(f)} ∘ ${prettyPrint(g)}"
  }
}
