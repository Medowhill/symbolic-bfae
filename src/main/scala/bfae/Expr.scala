package info.hjaem.bfae

import scala.util.parsing.combinator._

sealed trait Expr

case class Symbolic(x: Char) extends Expr
case class Num(n: BigInt) extends Expr
case class Add(l: Expr, r: Expr) extends Expr
case class If(c: Expr, t: Expr, f: Expr) extends Expr
case class Id(x: String) extends Expr
case class Val(x: String, e: Expr, b: Expr) extends Expr
case class Fun(xs: List[String], b: Expr) extends Expr
case class App(f: Expr, as: List[Expr]) extends Expr
case class NewBox(e: Expr) extends Expr
case class OpenBox(b: Expr) extends Expr
case class SetBox(b: Expr, e: Expr) extends Expr
case class Try(e: Expr, h: Expr) extends Expr
case class Seq(es: List[Expr]) extends Expr

object Expr extends RegexParsers {

  private def wrap[T](e: Parser[T]): Parser[T] = "(" ~> e <~ ")"

  private lazy val keywords: Set[String] =
    Set("if", "let", "fun", "ref", "get", "set", "seq", "try") ++
      ('A'.toInt to 'Z'.toInt).map(_.toChar.toString)

  private lazy val n: Parser[BigInt] =
    "-?[0-9]+".r ^^ BigInt.apply

  private lazy val sym: Parser[Char] =
    "[A-Z]".r ^^ (_.head)

  private lazy val x: Parser[String] =
    "[a-zA-Z_][a-zA-Z0-9_]*".r.withFilter(!keywords(_))

  private lazy val expr: Parser[Expr] =
    wrap("+" ~> expr ~ expr) ^^ { case l ~ r => Add(l, r) } |
    wrap("if" ~> expr ~ expr ~ expr) ^^ { case c ~ t ~ f => If(c, t, f) } |
    wrap("let" ~> wrap(x ~ expr) ~ expr) ^^ { case x ~ e ~ b => Val(x, e, b) } |
    wrap("fun" ~> wrap(rep(x)) ~ expr) ^^ { case xs ~ b => Fun(xs, b) } |
    wrap(expr ~ rep(expr)) ^^ { case f ~ as => App(f, as) } |
    wrap("ref" ~> expr) ^^ NewBox |
    wrap("get" ~> expr) ^^ OpenBox |
    wrap("set" ~> expr ~ expr) ^^ { case b ~ e => SetBox(b, e) } |
    wrap("try" ~> expr ~ expr) ^^ { case e ~ h => Try(e, h) } |
    wrap("seq" ~> rep1(expr)) ^^ Seq |
    n ^^ Num |
    sym ^^ Symbolic |
    x ^^ Id

  def apply(s: String): Expr = parseAll(expr, s).get
}
