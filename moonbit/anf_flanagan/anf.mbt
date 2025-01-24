///|
enum Expr {
  Int(Int)
  Var(String)
  Prim(String, Array[Expr])
  Lam(Array[String], Expr)
  App(Expr, Array[Expr])
  Let(String, Expr, Expr)
  If(Expr, Expr, Expr)
} derive(Show)

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
fn id[T](x : T) -> T {
  x
}

///|
fn is_value(e : Expr) -> Bool {
  match e {
    Int(_) => true
    Var(_) => true
    Lam(_) => true
    _ => false
  }
}

///|
fn normalize_name(e : Expr, k : (Expr) -> Expr) -> Expr {
  normalize(e, fn(ve) {
    if is_value(e) {
      k(e)
    } else {
      let t = gensym("t")
      Let(t, ve, k(Var(t)))
    }
  })
}

///|
fn normalize_term(e : Expr) -> Expr {
  normalize(e, id)
}

///|
fn normalize_names(es : ArrayView[Expr], k : (Array[Expr]) -> Expr) -> Expr {
  match es {
    [] => k([])
    [x, .. rest] =>
      normalize_name(x, fn(vx) {
        normalize_names(rest, fn(vrest) { k([vx, ..vrest]) })
      })
  }
}

///|
fn normalize(e : Expr, k : (Expr) -> Expr) -> Expr {
  match e {
    Int(_) | Var(_) => k(e)
    Lam(params, body) => k(Lam(params, normalize_term(body)))
    Let(name, value, body) =>
      normalize(value, fn(ve) { Let(name, ve, normalize(body, k)) })
    If(cond, then, orelse) =>
      normalize_name(cond, fn(vc) {
        k(If(vc, normalize(then, id), normalize(orelse, id)))
      })
    Prim(op, args) => normalize_names(args[:], fn(vargs) { k(Prim(op, vargs)) })
    App(f, args) =>
      normalize_name(f, fn(vf) {
        normalize_names(args[:], fn(vargs) { k(App(vf, vargs)) })
      })
  }
}

test "001" {
  reset()
  let e = Prim("+", [
    Prim("+", [Int(2), Int(2)]),
    Let("x", Int(1), App(Var("f"), [Var("x")])),
  ])
  let a = normalize_term(e)
  inspect!(
    a,
    content=
      #|Let("t0", Prim("+", [Int(2), Int(2)]), Let("x", Int(1), Let("t1", App(Var("f"), [Var("x")]), Prim("+", [Var("t0"), Var("t1")]))))
    ,
  )
}

test "002" {
  reset()
  let e = Prim("add1", [Let("x", App(Var("f"), [Int(5)]), Int(0))])
  let a = normalize_term(e)
  inspect!(
    a,
    content=
      #|Let("x", App(Var("f"), [Int(5)]), Let("t0", Int(0), Prim("add1", [Var("t0")])))
    ,
  )
}

test "003" {
  reset()
  let e = App(App(Var("f"), [Var("g")]), [App(Var("h"), [Var("x")]), Int(3)])
  let a = normalize_term(e)
  inspect!(
    a,
    content=
      #|Let("t0", App(Var("f"), [Var("g")]), Let("t1", App(Var("h"), [Var("x")]), App(Var("t0"), [Var("t1"), Int(3)])))
    ,
  )
}

test "004" {
  reset()
  let e : Expr = Let(
    "f",
    Lam(
      ["n"],
      If(
        Prim("=", [Var("n"), Int(0)]),
        Int(1),
        Prim("*", [Var("n"), App(Var("f"), [Prim("-", [Var("n"), Int(1)])])]),
      ),
    ),
    App(Var("f"), [Int(20)]),
  )
  let a = normalize_term(e)
  inspect!(
    a,
    content=
      #|Let("f", Lam(["n"], Let("t0", Prim("=", [Var("n"), Int(0)]), If(Var("t0"), Int(1), Let("t1", Prim("-", [Var("n"), Int(1)]), Let("t2", App(Var("f"), [Var("t1")]), Prim("*", [Var("n"), Var("t2")])))))), App(Var("f"), [Int(20)]))
    ,
  )
}

test "005" {
  reset()
  let e : Expr = Prim("*", [
    Prim("+", [Int(1), Int(2)]),
    Prim("+", [Int(3), Int(4)]),
  ])
  let a = normalize_term(e)
  inspect!(
    a,
    content=
      #|Let("t0", Prim("+", [Int(1), Int(2)]), Let("t1", Prim("+", [Int(3), Int(4)]), Prim("*", [Var("t0"), Var("t1")])))
    ,
  )
}

test "006" {
  reset()
  let e = Let(
    "x",
    If(
      If(
        Let("a", Int(1), Prim("<", [Var("a"), Int(2)])),
        Prim("+", [Int(3), Int(4)]),
        Prim("+", [Int(5), Int(6)]),
      ),
      If(
        Let("a", Int(7), Prim("<", [Var("a"), Int(8)])),
        Prim("+", [Int(9), Int(10)]),
        Prim("+", [Int(11), Int(12)]),
      ),
      If(
        Let("a", Int(13), Prim("<", [Var("a"), Int(14)])),
        Prim("+", [Int(15), Int(16)]),
        Prim("+", [Int(17), Int(18)]),
      ),
    ),
    Prim("+", [Var("x"), Int(1)]),
  )
  let a = normalize_term(e)
  inspect!(
    a,
    content=
      #|Let("a", Int(1), Let("t0", Prim("<", [Var("a"), Int(2)]), Let("t1", If(Var("t0"), Prim("+", [Int(3), Int(4)]), Prim("+", [Int(5), Int(6)])), Let("x", If(Var("t1"), Let("a", Int(7), Let("t2", Prim("<", [Var("a"), Int(8)]), If(Var("t2"), Prim("+", [Int(9), Int(10)]), Prim("+", [Int(11), Int(12)])))), Let("a", Int(13), Let("t3", Prim("<", [Var("a"), Int(14)]), If(Var("t3"), Prim("+", [Int(15), Int(16)]), Prim("+", [Int(17), Int(18)]))))), Prim("+", [Var("x"), Int(1)])))))
    ,
  )
}
