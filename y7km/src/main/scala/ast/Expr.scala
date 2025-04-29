package ast

sealed trait Expr[A] {
  def eval(x: A): Double
}

case class Const[A](value: Double) extends Expr[A] {
  def eval(x: A): Double = value
}

case class Id() extends Expr[Double] {
  def eval(x: Double): Double = x
}

case class Add[A](f: Expr[A], g: Expr[A]) extends Expr[A] {
  def eval(x: A): Double = f.eval(x) + g.eval(x)
}

case class Mul[A](f: Expr[A], g: Expr[A]) extends Expr[A] {
  def eval(x: A): Double = f.eval(x) * g.eval(x)
}

// (x, dx) => x
case class Proj1[A]() extends Expr[(A, A)] {
  def eval(x: (A, A)): Double = x._1.asInstanceOf[Double]
}

//(x, dx) => dx
case class Proj2[A]() extends Expr[(A, A)] {
  def eval(x: (A, A)): Double = x._2.asInstanceOf[Double]
}

// 합성: f(g(x))
case class Compose[A, B, C](f: Expr[B], g: Expr[A]) extends Expr[A] {
  def eval(x: A): Double = f.eval(g.eval(x).asInstanceOf[B])
}
