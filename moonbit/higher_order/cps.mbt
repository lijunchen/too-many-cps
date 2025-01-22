///|
enum Expr {
  Var(String)
  Lam(String, Expr)
  App(Expr, Expr)
} derive(Eq)

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
fn cps(e : Expr, k : (Expr) -> Expr) -> Expr {
  match e {
    Var(_) => k(e)
    Lam(x, m) => {
      let j = gensym("j")
      k(Lam(x, Lam(j, cps(m, fn(l) { App(Var(j), l) }))))
    }
    App(m, n) => {
      let a = gensym("a")
      cps(m, fn(vm) { cps(n, fn(vn) { App(App(vm, vn), Lam(a, k(Var(a)))) }) })
    }
  }
}

///|
fn id[T](x : T) -> T {
  x
}

test {
  reset()
  let e = Lam("f", Lam("y", Lam("x", App(Var("f"), App(Var("y"), Var("x"))))))
  let t = cps(e, id)
  inspect!(
    t,
    content="λf. λj0. (j0 λy. λj1. (j1 λx. λj2. ((y x) λa4. ((f a4) λa3. (j2 a3)))))",
  )
}

