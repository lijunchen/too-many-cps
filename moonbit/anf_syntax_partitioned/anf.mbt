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
enum ImmExpr {
  ImmInt(Int)
  ImmVar(String)
  ImmLam(Array[String], AExpr)
} derive(Show)

///|
enum CExpr {
  CImm(ImmExpr)
  CPrim(String, Array[ImmExpr])
  CApp(ImmExpr, Array[ImmExpr])
  CIf(ImmExpr, AExpr, AExpr)
} derive(Show)

///|
enum AExpr {
  ACExpr(CExpr)
  ALet(String, CExpr, AExpr)
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
fn aexpr_of_cexpr(c : CExpr) -> AExpr {
  ACExpr(c)
}

///|
fn normalize_term(e : Expr) -> AExpr {
  normalize(e, aexpr_of_cexpr)
}

///|
fn normalize(e : Expr, k : (CExpr) -> AExpr) -> AExpr {
  match e {
    Int(x) => k(CImm(ImmInt(x)))
    Var(x) => k(CImm(ImmVar(x)))
    Lam(params, body) => k(CImm(ImmLam(params, normalize_term(body))))
    Let(x, e, body) => normalize(e, fn(ve) { ALet(x, ve, normalize(body, k)) })
    If(cond, e1, e2) =>
      normalize_name(cond, fn(vc) {
        k(CIf(vc, normalize_term(e1), normalize_term(e2)))
      })
    Prim(op, args) =>
      normalize_names(args[:], fn(vargs) { k(CPrim(op, vargs)) })
    App(f, args) =>
      normalize_name(f, fn(vf) {
        normalize_names(args[:], fn(vargs) { k(CApp(vf, vargs)) })
      })
  }
}

///|
fn normalize_name(e : Expr, k : (ImmExpr) -> AExpr) -> AExpr {
  match e {
    Int(x) => k(ImmInt(x))
    Var(x) => k(ImmVar(x))
    _ => {
      let t = gensym("t")
      normalize(e, fn(ve) { ALet(t, ve, k(ImmVar(t))) })
    }
  }
}

///|
fn normalize_names(
  es : ArrayView[Expr],
  k : (Array[ImmExpr]) -> AExpr
) -> AExpr {
  match es {
    [] => k([])
    [x, .. rest] =>
      normalize_name(x, fn(vx) {
        normalize_names(rest, fn(vrest) { k([vx, ..vrest]) })
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
      #|ALet("t0", CPrim("+", [ImmInt(2), ImmInt(2)]), ALet("x", CImm(ImmInt(1)), ALet("t1", CApp(ImmVar("f"), [ImmVar("x")]), ACExpr(CPrim("+", [ImmVar("t0"), ImmVar("t1")])))))
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
      #|ALet("x", CApp(ImmVar("f"), [ImmInt(5)]), ALet("t0", CImm(ImmInt(0)), ACExpr(CPrim("add1", [ImmVar("t0")]))))
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
      #|ALet("t0", CApp(ImmVar("f"), [ImmVar("g")]), ALet("t1", CApp(ImmVar("h"), [ImmVar("x")]), ACExpr(CApp(ImmVar("t0"), [ImmVar("t1"), ImmInt(3)]))))
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
      #|ALet("f", CImm(ImmLam(["n"], ALet("t0", CPrim("=", [ImmVar("n"), ImmInt(0)]), ACExpr(CIf(ImmVar("t0"), ACExpr(CImm(ImmInt(1))), ALet("t2", CPrim("-", [ImmVar("n"), ImmInt(1)]), ALet("t1", CApp(ImmVar("f"), [ImmVar("t2")]), ACExpr(CPrim("*", [ImmVar("n"), ImmVar("t1")]))))))))), ACExpr(CApp(ImmVar("f"), [ImmInt(20)])))
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
      #|ALet("t0", CPrim("+", [ImmInt(1), ImmInt(2)]), ALet("t1", CPrim("+", [ImmInt(3), ImmInt(4)]), ACExpr(CPrim("*", [ImmVar("t0"), ImmVar("t1")]))))
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
      #|ALet("a", CImm(ImmInt(1)), ALet("t1", CPrim("<", [ImmVar("a"), ImmInt(2)]), ALet("t0", CIf(ImmVar("t1"), ACExpr(CPrim("+", [ImmInt(3), ImmInt(4)])), ACExpr(CPrim("+", [ImmInt(5), ImmInt(6)]))), ALet("x", CIf(ImmVar("t0"), ALet("a", CImm(ImmInt(7)), ALet("t2", CPrim("<", [ImmVar("a"), ImmInt(8)]), ACExpr(CIf(ImmVar("t2"), ACExpr(CPrim("+", [ImmInt(9), ImmInt(10)])), ACExpr(CPrim("+", [ImmInt(11), ImmInt(12)])))))), ALet("a", CImm(ImmInt(13)), ALet("t3", CPrim("<", [ImmVar("a"), ImmInt(14)]), ACExpr(CIf(ImmVar("t3"), ACExpr(CPrim("+", [ImmInt(15), ImmInt(16)])), ACExpr(CPrim("+", [ImmInt(17), ImmInt(18)]))))))), ACExpr(CPrim("+", [ImmVar("x"), ImmInt(1)]))))))
    ,
  )
}
