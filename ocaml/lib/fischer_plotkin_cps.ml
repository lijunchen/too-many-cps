(*
   Lambda Viewer
   https://projectultimatum.org/cgi-bin/lambda
*)
let gensym, reset =
  let counter = ref 0 in
  let gensym name =
    incr counter;
    name ^ "" ^ string_of_int !counter
  in
  let reset () = counter := 0 in
  (gensym, reset)

type var = string [@@deriving show { with_path = false }]

type expr = Var of var | Lam of var * expr | App of expr * expr
[@@deriving show { with_path = false }]

let rec string_of_expr e =
  match e with
  | Var x -> x
  | Lam (x, m) -> "λ" ^ x ^ "." ^ string_of_expr m
  | App (m, n) -> "(" ^ string_of_expr m ^ " " ^ string_of_expr n ^ ")"

let rec cps (e : expr) : expr =
  match e with
  | Var _ ->
      let k = gensym "k" in
      Lam (k, App (Var k, e))
  | Lam (x, m) ->
      let k = gensym "k" in
      Lam (k, App (Var k, Lam (x, cps m)))
  | App (m, n) ->
      let k = gensym "k" in
      let v1 = gensym "v" in
      let v2 = gensym "v" in
      Lam
        ( k,
          App
            ( cps m,
              Lam (v1, App (cps n, Lam (v2, App (App (Var v1, Var v2), Var k))))
            ) )

let%expect_test "001" =
  reset ();
  let e = Var "x" in
  let t = cps e in
  show_expr t |> print_endline;
  [%expect {| (Lam ("k1", (App ((Var "k1"), (Var "x"))))) |}];
  string_of_expr t |> print_endline;
  [%expect {| λk1.(k1 x) |}]

let%expect_test "002" =
  reset ();
  let e = Lam ("x", Var "x") in
  let t = cps e in
  show_expr t |> print_endline;
  [%expect
    {|
    (Lam ("k1",
       (App ((Var "k1"), (Lam ("x", (Lam ("k2", (App ((Var "k2"), (Var "x")))))))
          ))
       ))
    |}];
  string_of_expr t |> print_endline;
  [%expect {| λk1.(k1 λx.λk2.(k2 x)) |}]

let%expect_test "003" =
  reset ();
  let e = App (Var "f", Var "x") in
  let t = cps e in
  show_expr t |> print_endline;
  [%expect
    {|
    (Lam ("k1",
       (App ((Lam ("k5", (App ((Var "k5"), (Var "f"))))),
          (Lam ("v2",
             (App ((Lam ("k4", (App ((Var "k4"), (Var "x"))))),
                (Lam ("v3", (App ((App ((Var "v2"), (Var "v3"))), (Var "k1")))))
                ))
             ))
          ))
       ))
    |}];
  string_of_expr t |> print_endline;
  [%expect {| λk1.(λk5.(k5 f) λv2.(λk4.(k4 x) λv3.((v2 v3) k1))) |}]

let%expect_test "004" =
  reset ();
  let e = App (Lam ("x", Var "x"), Var "y") in
  let t = cps e in
  show_expr t |> print_endline;
  [%expect
    {|
    (Lam ("k1",
       (App (
          (Lam ("k5",
             (App ((Var "k5"),
                (Lam ("x", (Lam ("k6", (App ((Var "k6"), (Var "x")))))))))
             )),
          (Lam ("v2",
             (App ((Lam ("k4", (App ((Var "k4"), (Var "y"))))),
                (Lam ("v3", (App ((App ((Var "v2"), (Var "v3"))), (Var "k1")))))
                ))
             ))
          ))
       ))
    |}];
  string_of_expr t |> print_endline;
  [%expect {| λk1.(λk5.(k5 λx.λk6.(k6 x)) λv2.(λk4.(k4 y) λv3.((v2 v3) k1))) |}]

let%expect_test "005" =
  reset ();
  let e =
    Lam ("f", Lam ("x", Lam ("y", App (App (Var "f", Var "y"), Var "x"))))
  in
  string_of_expr e |> print_endline;
  [%expect {| λf.λx.λy.((f y) x) |}];
  let t = cps e in
  string_of_expr t |> print_endline;
  [%expect
    {| λk1.(k1 λf.λk2.(k2 λx.λk3.(k3 λy.λk4.(λk8.(λk12.(k12 f) λv9.(λk11.(k11 y) λv10.((v9 v10) k8))) λv5.(λk7.(k7 x) λv6.((v5 v6) k4)))))) |}]

let%expect_test "006" =
  (* motivation case for Clinger's properly tail-recursive CPS transform *)
  reset ();
  let e = Lam ("f", App (Var "f", Var "x")) in
  string_of_expr e |> print_endline;
  [%expect {| λf.(f x) |}];
  let t = cps e in
  string_of_expr t |> print_endline;
  [%expect {| λk1.(k1 λf.λk2.(λk6.(k6 f) λv3.(λk5.(k5 x) λv4.((v3 v4) k2)))) |}]
