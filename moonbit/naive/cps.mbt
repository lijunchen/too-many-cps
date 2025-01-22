






enum Expr {
  Var(String)
  Lam(String, Expr)
  App(Expr, Expr)
} derive(ToJson, Eq)

///|
impl Show for Expr with output(self : Expr, logger : &Logger) {
  match self {
    Var(x) => logger.write_string(x)
    Lam(name, body) => logger.write_string("λ\{name}. \{body}")
    App(m, n) => logger.write_string("(\{m} \{n})")
  }
}

///|
let counter : Ref[Int] = @ref.new(0)

///|
fn gensym(name : String) -> String {
  let n = counter.val
  counter.val += 1
  "\{name}\{n}"
}

///|
fn reset() -> Unit {
  counter.val = 0
}

///|
fn cps(e : Expr) -> Expr {
  match e {
    Var(_) => {
      let k = gensym("k")
      Lam(k, App(Var(k), e))
    }
    Lam(x, m) => {
      let k = gensym("k")
      Lam(k, App(Var(k), Lam(x, cps(m))))
    }
    App(m, n) => {
      let k = gensym("k")
      let v1 = gensym("v")
      let v2 = gensym("v")
      Lam(
        k,
        App(
          cps(m),
          Lam(v1, App(cps(n), Lam(v2, App(App(Var(v1), Var(v2)), Var(k))))),
        ),
      )
    }
  }
}

test {
  reset()
  let e = Lam("f", Lam("y", Lam("x", App(Var("f"), App(Var("y"), Var("x"))))))
  let t = cps(e)
  inspect!(
    t,
    content="λk0. (k0 λf. λk1. (k1 λy. λk2. (k2 λx. λk3. (λk6. (k6 f) λv4. (λk7. (λk10. (k10 y) λv8. (λk11. (k11 x) λv9. ((v8 v9) k7))) λv5. ((v4 v5) k3))))))",
  )
}