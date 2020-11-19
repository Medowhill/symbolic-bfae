package info.hjaem.bfae

import SymbolicValue._

object SymbolicInterpreter {

  type Sto = Map[Int, SymbolicValue]

  case class Interp[+A](
    value: (Sto, Constraint) => List[(Either[String, A], Sto, Constraint)]
  ) {

    def flatMap[B](f: A => Interp[B]): Interp[B] = Interp(
      (sto, cst) => this(sto, cst).flatMap{
        case (Left(l), sto, cst) => (Left(l), sto, cst) :: Nil
        case (Right(a), sto, cst) => f(a)(sto, cst)
      }
    )

    def map[B](f: A => B): Interp[B] = Interp(
      (sto, cst) => this(sto, cst).map{
        case (Left(l), sto, cst) => (Left(l), sto, cst)
        case (Right(a), sto, cst) => (Right(f(a)), sto, cst)
      }
    )

    def ++[AA >: A](that: Interp[AA]): Interp[AA] = Interp(
      (sto, cst) => this(sto, cst) ++ that(sto, cst)
    )

    def apply(sto: Sto, cst: Constraint) = value(sto, cst)
  }

  def pure[A](a: A): Interp[A] =
    Interp((sto, cst) => (Right(a), sto, cst) :: Nil)

  def err(msg: String): Interp[Nothing] =
    Interp((sto, cst) => (Left(msg), sto, cst) :: Nil)

  def getInt(v: SymbolicValue): Interp[IntFormula] = v match {
    case NumV(n) => pure(n)
    case _ => err("not a number")
  }

  def getClosure(v: SymbolicValue): Interp[CloV] = v match {
    case v: CloV => pure(v)
    case _ => err("not a closure")
  }

  def getAddr(v: SymbolicValue): Interp[Int] = v match {
    case BoxV(a) => pure(a)
    case _ => err("not a box")
  }

  val get: Interp[Sto] =
    Interp((sto, cst) => (Right(sto), sto, cst) :: Nil)

  def set(a: Int, v: SymbolicValue): Interp[Unit] =
    Interp((sto, cst) => (Right(()), sto + (a -> v), cst) :: Nil)

  def cstrain[A](l: (A, Constraint)*): Interp[A] =
    Interp((sto, cst) => l.toList.map{
      case (a, c) => (Right(a), sto, cst && c)
    })

  def fromOpt[A](opt: Option[A], msg: String): Interp[A] =
    Interp((sto, cst) => opt match {
      case Some(v) => (Right(v), sto, cst) :: Nil
      case None => (Left(msg), sto, cst) :: Nil
    })

  def interp(expr: Expr): List[(Either[String, SymbolicValue], Sto, Constraint)] =
    interp(expr, Map())(Map(), True)

  def interp(expr: Expr, env: Env): Interp[SymbolicValue] = expr match {
    case Symbolic(x) => pure(NumV(Symbol(x)))
    case Num(n) => pure(NumV(Constant(n)))
    case Add(l, r) =>
      for (
        v1 <- interp(l, env);
        v2 <- interp(r, env);
        n1 <- getInt(v1);
        n2 <- getInt(v2)
      ) yield NumV(n1 + n2)
    case If(c, t, f) =>
      for (
        cv <- interp(c, env);
        c <- getInt(cv);
        e <- cstrain(
          (t, c !== IntFormula.Zero),
          (f, c === IntFormula.Zero)
        );
        v <- interp(e, env)
      ) yield v
    case Id(x) =>
      fromOpt(env.get(x), s"free id ($x)")
    case Val(x, e, b) =>
      for (
        v1 <- interp(e, env);
        v2 <- interp(b, env + (x -> v1))
      ) yield v2
    case Fun(xs, b) => pure(CloV(xs, b, env))
    case App(f, as) =>
      for (
        fv <- interp(f, env);
        avs <- interpMultiple(as, env);
        clov <- getClosure(fv);
        v <- {
          val CloV(xs, b, fenv) = clov
          if (xs.length == avs.length)
            interp(b, fenv ++ xs.zip(avs))
          else
            err("arity mismatch")
        }
      ) yield v
    case NewBox(e) =>
      for (
        v <- interp(e, env);
        sto <- get;
        a <- pure((sto.keySet + 0).max + 1);
        _ <- set(a, v)
      ) yield BoxV(a)
    case OpenBox(b) =>
      for (
        bv <- interp(b, env);
        a <- getAddr(bv);
        sto <- get;
        v <- fromOpt(sto.get(a), s"unknown addr: $a")
      ) yield v
    case SetBox(b, e) =>
      for (
        bv <- interp(b, env);
        v <- interp(e, env);
        a <- getAddr(bv);
        _ <- set(a, v)
      ) yield v
    case Try(e, h) =>
      val i = interp(e, env)
      Interp((sto, cst) => {
        i(sto, cst).flatMap{
          case (l @ Left(_), sto, cst) => interp(h, env)(sto, cst)
          case (r @ Right(_), sto, cst) => (r, sto, cst) :: Nil
        }
      })
    case Seq(es) =>
      for (
        vs <- interpMultiple(es, env)
      ) yield vs.last
  }

  def interpMultiple(exprs: List[Expr], env: Env): Interp[List[SymbolicValue]] =
    exprs match {
      case Nil => pure(Nil)
      case h :: t =>
        for (
          hv <- interp(h, env);
          tvs <- interpMultiple(t, env)
        ) yield hv :: tvs
    }
}
