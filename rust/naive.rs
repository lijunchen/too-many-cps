#![allow(unused)]

use std::sync::atomic::{AtomicU32, Ordering};

static COUNTER: AtomicU32 = AtomicU32::new(0);

fn gensym(name: &str) -> String {
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

fn var(x: &str) -> Expr {
    Expr::Var(x.to_string())
}

fn lam(x: &str, m: Expr) -> Expr {
    Expr::Lam(x.to_string(), Box::new(m))
}

fn app(m: Expr, n: Expr) -> Expr {
    Expr::App(Box::new(m), Box::new(n))
}

fn check(actual: &Expr, expect: expect_test::Expect) {
    expect.assert_eq(&format!("{:?}", actual));
}

use Expr::*;

fn cps(e: &Expr) -> Expr {
    match e {
        Var(_) => {
            let k = gensym("k");
            lam(&k, app(var(&k), e.clone()))
        }
        Lam(x, m) => {
            let k = gensym("k");
            lam(&k, app(var(&k), lam(x, cps(m))))
        }
        App(m, n) => {
            let k = gensym("k");
            let vm = gensym("v");
            let vn = gensym("v");
            lam(
                &k,
                app(
                    cps(m),
                    lam(
                        &vm,
                        app(cps(n), lam(&vn, app(app(var(&vm), var(&vn)), var(&k)))),
                    ),
                ),
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::expect;

    #[test]
    fn test_001() {
        reset();
        let e = var("x");
        let t = cps(&e);
        check(&t, expect!["λk0.(k0 x)"]);
    }

    #[test]
    fn test_002() {
        reset();
        let e = lam("x", var("x"));
        let t = cps(&e);
        check(&t, expect!["λk0.(k0 λx.λk1.(k1 x))"]);
    }

    #[test]
    fn test_003() {
        reset();
        let e = app(var("f"), var("x"));
        let t = cps(&e);
        check(
            &t,
            expect!["λk0.(λk3.(k3 f) λv1.(λk4.(k4 x) λv2.((v1 v2) k0)))"],
        );
    }

    #[test]
    fn test_004() {
        reset();
        let e = app(lam("x", var("x")), var("y"));
        let t = cps(&e);
        check(
            &t,
            expect!["λk0.(λk3.(k3 λx.λk4.(k4 x)) λv1.(λk5.(k5 y) λv2.((v1 v2) k0)))"],
        );
    }

    #[test]
    fn test_005() {
        reset();
        let e = lam(
            "f",
            lam("x", lam("y", app(app(var("f"), var("y")), var("x")))),
        );
        let t = cps(&e);
        check(&t, expect!["λk0.(k0 λf.λk1.(k1 λx.λk2.(k2 λy.λk3.(λk6.(λk9.(k9 f) λv7.(λk10.(k10 y) λv8.((v7 v8) k6))) λv4.(λk11.(k11 x) λv5.((v4 v5) k3))))))"]);
    }

    #[test]
    fn test_006() {
        reset();
        let e = lam("f", app(var("f"), var("x")));
        let t = cps(&e);
        check(
            &t,
            expect!["λk0.(k0 λf.λk1.(λk4.(k4 f) λv2.(λk5.(k5 x) λv3.((v2 v3) k1))))"],
        );
    }
}
