package info.hjaem.bfae

object Main {
  def main(args: Array[String]): Unit = {

    // val e = Expr("""
    //   (let (inc (fun (x v) (set x (+ v (get x)))))
    //   (let (x (ref A))
    //   (let (y B)
    //     (seq
    //       (inc x 3)
    //       (if
    //         (get x)
    //         (if y (+ 0 1) ((fun () 0) 1))
    //         (seq
    //           (inc x 2)
    //           (get x)
    //         )
    //       )
    //     )
    //   )))
    // """)

    val e = Expr("""
      (let (err (fun () (1 1)))
      (let (x A)
      (let (y B)
        (if (+ x 3)
          (try (if
            (+ x y)
            (if (+ x 1) (err) 0)
            (if (+ y 2) 1 (err))
          ) 3)
          (err)
        )
      )))
    """)

    val res = SymbolicInterpreter.interp(e)

    println(e)
    println()
    res foreach println
  }
}
