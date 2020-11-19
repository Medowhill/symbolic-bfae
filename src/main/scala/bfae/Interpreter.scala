package info.hjaem.bfae

import Value._

object Interpreter {

  type Sto = Map[Int, Value]

  case class Interp[+A](
    value: Sto => (Either[String, A], Sto)
  ) {

    def flatMap[B](f: A => Interp[B]): Interp[B] = Interp(
      sto => this(sto) match {
        case (Left(l), sto) => (Left(l), sto)
        case (Right(a), sto) => f(a)(sto)
      }
    )

    def map[B](f: A => B): Interp[B] = Interp(
      sto => this(sto) match {
        case (Left(l), sto) => (Left(l), sto)
        case (Right(a), sto) => (Right(f(a)), sto)
      }
    )

    def apply(sto: Sto) = value(sto)
  }

  def pure[A](a: A): Interp[A] =
    Interp(sto => (Right(a), sto))

  def err(msg: String): Interp[Nothing] =
    Interp(sto => (Left(msg), sto))

  def getInt(v: Value): Interp[BigInt] = v match {
    case NumV(n) => pure(n)
    case _ => err(s"not a number ($v)")
  }

  def getClosure(v: Value): Interp[CloV] = v match {
    case v: CloV => pure(v)
    case _ => err(s"not a closure ($v)")
  }

  def getAddr(v: Value): Interp[Int] = v match {
    case BoxV(a) => pure(a)
    case _ => err(s"not a box ($v)")
  }

  val get: Interp[Sto] = Interp(sto => (Right(sto), sto))

  def set(a: Int, v: Value): Interp[Unit] =
    Interp(sto => (Right(()), sto + (a -> v)))

  def fromOpt[A](opt: Option[A], msg: String): Interp[A] =
    Interp(sto => opt match {
      case Some(v) => (Right(v), sto)
      case None => (Left(msg), sto)
    })

  def interp(expr: Expr): (Either[String, Value], Sto) =
    interp(expr, Map())(Map())

  def interp(expr: Expr, env: Env): Interp[Value] = expr match {
    case Num(n) => pure(NumV(n))
    case Add(l, r) =>
      for (
        v1 <- interp(l, env);
        v2 <- interp(r, env);
        n1 <- getInt(v1);
        n2 <- getInt(v2)
      ) yield NumV(n1 + n2)
    case If(c, t, f) =>
      for (
        v1 <- interp(c, env);
        n <- getInt(v1);
        v2 <- interp(if (n != 0) t else f, env)
      ) yield v2
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
      Interp(sto => {
        i(sto) match {
          case (l @ Left(_), sto) => interp(h, env)(sto)
          case (r @ Right(_), sto) => (r, sto)
        }
      })
    case Seq(es) =>
      for (
        vs <- interpMultiple(es, env)
      ) yield vs.last
    case _: Symbolic => sys.error("symbol not supported")
  }

  def interpMultiple(exprs: List[Expr], env: Env): Interp[List[Value]] =
    exprs match {
      case Nil => pure(Nil)
      case h :: t =>
        for (
          hv <- interp(h, env);
          tvs <- interpMultiple(t, env)
        ) yield hv :: tvs
    }
}
