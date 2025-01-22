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

let rec cps (e : expr) (k : expr -> expr) : expr =
  match e with
  | Var _ -> k e
  | Lam (x, m) ->
      let j = gensym "j" in
      k (Lam (x, Lam (j, cps' m (Var j))))
  | App (m, n) ->
      let a = gensym "a" in
      cps m (fun vm -> cps n (fun vn -> App (App (vm, vn), Lam (a, k (Var a)))))

and cps' (e : expr) (k : expr) : expr =
  match e with
  | Var _ -> App(k, e)
  | Lam(x, m) ->
    let j = gensym "j" in
    App(k, Lam(x, Lam(j, cps' m (Var j))))
  | App(m, n) ->
    cps m (fun vm -> cps n (fun vn -> App(App(vm, vn), k)))


let id x = x

let%expect_test _ =
  reset ();
  let e = Var "x" in
  let t = cps e id in
  show_expr t |> print_endline;
  [%expect {| (Var "x") |}];
  string_of_expr t |> print_endline;
  [%expect {| x |}]

let%expect_test _ =
  reset ();
  let e = Lam ("x", Var "x") in
  let t = cps e id in
  show_expr t |> print_endline;
  [%expect {| (Lam ("x", (Lam ("j1", (App ((Var "j1"), (Var "x"))))))) |}];
  string_of_expr t |> print_endline;
  [%expect {| λx.λj1.(j1 x) |}]

let%expect_test _ =
  reset ();
  let e = App (Var "f", Var "x") in
  let t = cps e id in
  show_expr t |> print_endline;
  [%expect {| (App ((App ((Var "f"), (Var "x"))), (Lam ("a1", (Var "a1"))))) |}];
  string_of_expr t |> print_endline;
  [%expect {| ((f x) λa1.a1) |}]

let%expect_test _ =
  reset ();
  let e = App (Lam ("x", Var "x"), Var "y") in
  let t = cps e id in
  show_expr t |> print_endline;
  [%expect
    {|
    (App (
       (App ((Lam ("x", (Lam ("j2", (App ((Var "j2"), (Var "x"))))))), (Var "y")
          )),
       (Lam ("a1", (Var "a1")))))
    |}];
  string_of_expr t |> print_endline;
  [%expect {| ((λx.λj2.(j2 x) y) λa1.a1) |}]

let%expect_test _ =
  reset ();
  let e =
    Lam ("f", Lam ("x", Lam ("y", App (App (Var "f", Var "y"), Var "x"))))
  in
  string_of_expr e |> print_endline;
  [%expect {| λf.λx.λy.((f y) x) |}];
  let t = cps e id in
  string_of_expr t |> print_endline;
  [%expect
    {| λf.λj1.(j1 λx.λj2.(j2 λy.λj3.((f y) λa4.((a4 x) j3)))) |}]

let%expect_test _ =
  reset ();
  let e = Lam ("f", App (Var "f", Var "x")) in
  string_of_expr e |> print_endline;
  [%expect {| λf.(f x) |}];
  let t = cps e id in
  string_of_expr t |> print_endline;
  [%expect {| λf.λj1.((f x) j1) |}]
