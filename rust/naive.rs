#![allow(unused)]

static mut COUNTER: u32 = 0;

fn gensym(name: &str) -> String {
    unsafe {
        COUNTER += 1;
        format!("{}{}", name, COUNTER)
    }
}

fn reset() {
    unsafe {
        COUNTER = 0;
    }
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
        check(&t, expect!["λk1.(k1 x)"]);
    }

    #[test]
    fn test_002() {
        reset();
        let e = lam("x", var("x"));
        let t = cps(&e);
        check(&t, expect!["λk1.(k1 λx.λk2.(k2 x))"]);
    }

    #[test]
    fn test_003() {
        reset();
        let e = app(var("f"), var("x"));
        let t = cps(&e);
        check(
            &t,
            expect!["λk1.(λk4.(k4 f) λv2.(λk5.(k5 x) λv3.((v2 v3) k1)))"],
        );
    }

    #[test]
    fn test_004() {
        reset();
        let e = app(lam("x", var("x")), var("y"));
        let t = cps(&e);
        check(
            &t,
            expect!["λk1.(λk4.(k4 λx.λk5.(k5 x)) λv2.(λk6.(k6 y) λv3.((v2 v3) k1)))"],
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
        check(&t, expect!["λk1.(k1 λf.λk2.(k2 λx.λk3.(k3 λy.λk4.(λk7.(λk10.(k10 f) λv8.(λk11.(k11 y) λv9.((v8 v9) k7))) λv5.(λk12.(k12 x) λv6.((v5 v6) k4))))))"]);
    }

    #[test]
    fn test_006() {
        reset();
        let e = lam("f", app(var("f"), var("x")));
        let t = cps(&e);
        check(
            &t,
            expect!["λk1.(k1 λf.λk2.(λk5.(k5 f) λv3.(λk6.(k6 x) λv4.((v3 v4) k2))))"],
        );
    }
}
