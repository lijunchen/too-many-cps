#![allow(unused)]

use std::sync::atomic::{AtomicU32, Ordering};

static COUNTER: AtomicU32 = AtomicU32::new(0);

fn gensym(name: &str) -> String {
    let old = COUNTER.load(Ordering::SeqCst);
    let count = COUNTER.fetch_add(1, Ordering::SeqCst);
    format!("{}{}", name, count)
}

fn reset() {
    COUNTER.store(0, Ordering::SeqCst);
}

#[derive(Clone)]
enum Expr {
    Var(String),
    Lam(String, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Var(x) => write!(f, "{}", x),
            Lam(x, m) => write!(f, "λ{}.{:?}", x, m),
            App(m, n) => write!(f, "({:?} {:?})", m, n),
        }
    }
}

fn var<S: AsRef<str>>(x: S) -> Expr {
    Expr::Var(x.as_ref().to_string())
}

fn lam<S: AsRef<str>>(x: S, m: Expr) -> Expr {
    Expr::Lam(x.as_ref().to_string(), Box::new(m))
}

fn app(m: Expr, n: Expr) -> Expr {
    Expr::App(Box::new(m), Box::new(n))
}

fn check(actual: &Expr, expect: expect_test::Expect) {
    expect.assert_eq(&format!("{:?}", actual));
}

use Expr::*;

fn cps(e: Expr, k: Box<dyn FnOnce(Expr) -> Expr>) -> Expr {
    match e {
        Var(_) => k(e),
        Lam(x, m) => {
            let j = gensym("j");
            k(lam(
                x,
                lam(j.clone(), cps(*m, Box::new(move |l| app(var(j), l)))),
            ))
        }
        App(m, n) => {
            let a = gensym("a");
            cps(
                *m,
                Box::new(move |vm| {
                    cps(
                        *n,
                        Box::new(move |vn| app(app(vm, vn), lam(a.clone(), k(var(a))))),
                    )
                }),
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::expect;

    fn id<T>(x: T) -> T {
        x
    }

    #[test]
    fn test_001() {
        reset();
        let e = var("x");
        let t = cps(e, Box::new(id));
        check(&t, expect!["x"]);
    }

    #[test]
    fn test_002() {
        reset();
        let e = lam("x", var("x"));
        let t = cps(e, Box::new(id));
        check(&t, expect!["λx.λj1.(j1 x)"]);
    }

    #[test]
    fn test_003() {
        reset();
        let e = app(var("f"), var("x"));
        let t = cps(e, Box::new(id));
        check(&t, expect!["((f x) λa1.a1)"]);
    }

    #[test]
    fn test_004() {
        reset();
        let e = app(lam("x", var("x")), var("y"));
        let t = cps(e, Box::new(id));
        check(&t, expect!["((λx.λj2.(j2 x) y) λa1.a1)"]);
    }

    #[test]
    fn test_005() {
        reset();
        let e = lam(
            "f",
            lam("x", lam("y", app(app(var("f"), var("y")), var("x")))),
        );
        let t = cps(e, Box::new(id));
        check(
            &t,
            expect!["λf.λj1.(j1 λx.λj2.(j2 λy.λj3.((f y) λa5.((a5 x) λa4.(j3 a4)))))"],
        );
    }

    #[test]
    fn test_006() {
        reset();
        let e = lam("f", app(var("f"), var("x")));
        let t = cps(e, Box::new(id));
        check(&t, expect!["λf.λj1.((f x) λa2.(j1 a2))"]);
    }
}
