package info.hjaem.bfae

sealed trait Value

object Value {

  type Env = Map[String, Value]

  case class NumV(n: BigInt) extends Value
  case class CloV(xs: List[String], b: Expr, env: Env) extends Value
  case class BoxV(a: Int) extends Value
}

sealed trait SymbolicValue

object SymbolicValue {

  type Env = Map[String, SymbolicValue]

  case class NumV(n: IntFormula) extends SymbolicValue
  case class CloV(xs: List[String], b: Expr, env: Env) extends SymbolicValue
  case class BoxV(a: Int) extends SymbolicValue
}

sealed trait IntFormula {
  def +(that: IntFormula): IntFormula = Sum(this, that)
  def ===(that: IntFormula): Constraint = Eq(this, that)
  def !==(that: IntFormula): Constraint = Neq(this, that)
}

object IntFormula {
  val Zero = Constant(0)
}

case class Symbol(x: Char) extends IntFormula {
  override def toString: String = x.toString
}
case class Constant(v: BigInt) extends IntFormula {
  override def toString: String = v.toString
}
case class Sum(l: IntFormula, r: IntFormula) extends IntFormula {
  override def toString: String = s"$l + $r"
}

sealed trait Constraint {
  def &&(that: Constraint): Constraint = And(this, that)
}

case object True extends Constraint
case class Eq(l: IntFormula, r: IntFormula) extends Constraint {
  override def toString: String = s"$l == $r"
}
case class Neq(l: IntFormula, r: IntFormula) extends Constraint {
  override def toString: String = s"$l != $r"
}
case class And(l: Constraint, r: Constraint) extends Constraint {
  override def toString: String = s"$l /\\ $r"
}
