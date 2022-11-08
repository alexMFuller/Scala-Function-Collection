abstract class Exp {
  // return the string representation of this expression
  override def toString: String = this match {
    // TODO
    case Num(x)        => x.toString()
    case Plus(e1, e2)  => "(" + e1.toString() + " + " + e2.toString() + ")"
    case Minus(e1, e2) => "(" + e1.toString() + " - " + e2.toString() + ")"
    case Div(e1, e2)   => "(" + e1.toString() + " / " + e2.toString() + ")"
    case Times(e1, e2) => "(" + e1.toString() + " * " + e2.toString() + ")"
    case Var(x)        => x
    case Let(x, e1, e2) =>
      "Let val " + x + " = " + (e1).toString + " in " + e2.toString + " end"
    case Fn(x, e)    => "fn " + x + " =>" + e.toString()
    case App(e1, e2) => e1.toString() + " " + e2.toString()

  }

  // lookup 'y' in 'ctx'
  def lookup(ctx: List[(String, Value)], y: String): Value = {

    ctx match {
      // TODO

      case ((x, v) :: ctx) =>
        if (x.equals(y.toString())) {
          v
        } else {
          lookup(ctx, y)
        }

      case nil => {

        Error("Variable " + y + " is not defined")
      }

    }
  }

  def eval: Value = this eval List()

  // evaluate 'this' expression with the context 'ctx'
  def eval(ctx: List[(String, Value)]): Value = {

    this match {
      // TODO

      case Var(x) => lookup(ctx, x)
      case Num(x) => CVal(x)

      case Plus(e1, e2) => {
        val x = e1.eval(ctx)
        val y = e2.eval(ctx)
        if (x.isInstanceOf[Error]) {
          x
        } else if (y.isInstanceOf[Error]) {
          y
        } else if (!x.isInstanceOf[CVal]) {
          Error("Plus Error:" + x.toString + " is not a number")
        } else if (!y.isInstanceOf[CVal]) {
          Error("Plus Error:" + y.toString + " is not a number")
        } else {
          CVal(x.toString.toInt + y.toString.toInt)
        }

      }

      case Minus(e1, e2) => {
        val x = e1.eval(ctx)
        val y = e2.eval(ctx)
        if (x.isInstanceOf[Error]) {
          x
        } else if (y.isInstanceOf[Error]) {
          y
        } else if (!x.isInstanceOf[CVal]) {
          Error("Minus Error:" + x.toString + " is not a number")
        } else if (!y.isInstanceOf[CVal]) {
          Error("Minus Error:" + y.toString + " is not a number")
        } else {
          CVal(x.toString.toInt - y.toString.toInt)
        }
      }

      case Div(e1, e2) => {
        val x = e1.eval(ctx)
        val y = e2.eval(ctx)
        if (x.isInstanceOf[Error]) {
          x
        } else if (y.isInstanceOf[Error]) {
          y
        } else if (!x.isInstanceOf[CVal]) {
          Error(x.toString + " is not a number")
        } else if (!y.isInstanceOf[CVal]) {
          Error(y.toString + " is not a number")
        } else if (y.toString.toInt == 0) {
          Error("Division by zero error in: " + x.toString + "/" + y.toString)
        } else {
          CVal(x.toString.toInt / y.toString.toInt)
        }
      }

      case Times(e1, e2) => {
        val x = e1.eval(ctx)
        val y = e2.eval(ctx)
        if (x.isInstanceOf[Error]) {
          x
        } else if (y.isInstanceOf[Error]) {
          y
        } else if (!x.isInstanceOf[CVal]) {
          Error("Times Error:" + x.toString + " is not a number")
        } else if (!y.isInstanceOf[CVal]) {
          Error("Times Error:" + y.toString + " is not a number")
        } else {
          CVal(x.toString.toInt * y.toString.toInt)
        }
      }

      case Let(x, e1, e2) => {

        if (e1.eval.isInstanceOf[Error]) {
          e1.eval(ctx)
        } else {
          e2.eval((x, e1.eval(ctx)) :: ctx)
        }

      }

      case App(e1, e2) => {
        val z = e1.eval(ctx)
        val y = e2.eval(ctx)

        if (z.isInstanceOf[CVal]) {
          Error("Application Error: " + z + " is not a function")
        } else if (z.isInstanceOf[Error]) {
          Error("Application Error: " + z + " is not a function")
        } else {
          val FnVal(x, e, ctx1) = e1.eval(ctx)
          val v = e2.eval(ctx)
          e.eval(((x, v) :: ctx1))
        }

      }

      case Fn(x, e) => {
        FnVal(x, e, ctx)
      }

    }
  }
}

case class Num(x: Int) extends Exp
case class Plus(e1: Exp, e2: Exp) extends Exp
case class Minus(e1: Exp, e2: Exp) extends Exp
case class Times(e1: Exp, e2: Exp) extends Exp
case class Div(e1: Exp, e2: Exp) extends Exp
case class Var(x: String) extends Exp
case class Let(x: String, e1: Exp, e2: Exp) extends Exp
case class App(e1: Exp, e2: Exp) extends Exp
case class Fn(x: String, e: Exp) extends Exp

abstract class Value {
  // return the string representation of this value
  override def toString = this match {
    // TODO
    case CVal(x)          => x.toString()
    case Error(m)         => m
    case FnVal(x, e, ctx) => "(fn " + x + " => " + e.toString() + ")"
  }
}
case class CVal(x: Int) extends Value
case class FnVal(x: String, e: Exp, ctx: List[(String, Value)]) extends Value
case class Error(m: String) extends Value

object Hwk9 {
  def main(args: Array[String]): Unit = {
    val t1 = Let(
      "y",
      Num(10),
      Let(
        "f",
        Fn("x", Plus(Var("x"), Var("y"))),
        Let("y", Num(20), App(Var("f"), Num(5)))
      )
    )

    val t2 = Let(
      "y",
      Num(10),
      Let(
        "f",
        Fn("x", Plus(Var("x"), Var("z"))),
        Let("y", Num(20), App(Var("f"), Num(5)))
      )
    )

    val t3 = Div(Num(10), Num(0))

    val t4 = Plus(Num(10), Minus(Num(10), Fn("x", Num(5))))

    val t5 = App(Num(10), Num(10))

    val t6 = Let("f", Num(10), App(Var("f"), Num(20)))

    val t7 = Let("x", Plus(Num(10), Fn("x", Var("x"))), Plus(Num(0), Num(20)))

    val t9 = Let("y", Num(10), Let("y", Num(20), Times(Var("y"), Num(5))));

    println(t1.eval)
    println(t2.eval)
    println(t3.eval)
    println(t4.eval)
    println(t5.eval)
    println(t6.eval)
    println(t7.eval)
  }
}
