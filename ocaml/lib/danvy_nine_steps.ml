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

(* Fischer and Plotkin's naive CPS *)
let rec cps0 (e : expr) : expr =
  match e with
  | Var _ ->
      let k = gensym "k" in
      Lam (k, App (Var k, e))
  | Lam (x, m) ->
      let k = gensym "k" in
      Lam (k, App (Var k, Lam (x, cps0 m)))
  | App (m, n) ->
      let k = gensym "k" in
      let v1 = gensym "v" in
      let v2 = gensym "v" in
      Lam
        ( k,
          App
            ( cps0 m,
              Lam (v1, App (cps0 n, Lam (v2, App (App (Var v1, Var v2), Var k))))
            ) )

(* Q1~3，eta expansion on Lam *)
let rec cps1 (e : expr) : expr =
  match e with
  | Var _ ->
      let k = gensym "k" in
      Lam (k, App (Var k, e))
  | Lam (x, m) ->
      let k = gensym "k" in
      let j = gensym "j" in
      Lam (k, App (Var k, Lam (x, Lam (j, App (cps1 m, Var j)))))
  | App (m, n) ->
      let k = gensym "k" in
      let v1 = gensym "v" in
      let v2 = gensym "v" in
      Lam
        ( k,
          App
            ( cps1 m,
              Lam (v1, App (cps1 n, Lam (v2, App (App (Var v1, Var v2), Var k))))
            ) )

(* Q4~6，eta expansion on Lam again *)
let rec cps2 (e : expr) : expr =
  match e with
  | Var _ ->
      let k = gensym "k" in
      Lam (k, App (Var k, e))
  | Lam (x, m) ->
      let k = gensym "k" in
      let j = gensym "j" in
      let l = gensym "l" in
      Lam
        ( k,
          App
            (Var k, Lam (x, Lam (j, App (cps2 m, Lam (l, App (Var j, Var l))))))
        )
  | App (m, n) ->
      let k = gensym "k" in
      let v1 = gensym "v" in
      let v2 = gensym "v" in
      Lam
        ( k,
          App
            ( cps2 m,
              Lam (v1, App (cps2 n, Lam (v2, App (App (Var v1, Var v2), Var k))))
            ) )

(* Q7~9，eta expansion on App *)
let rec cps3 (e : expr) : expr =
  match e with
  | Var _ ->
      let k = gensym "k" in
      Lam (k, App (Var k, e))
  | Lam (x, m) ->
      let k = gensym "k" in
      let j = gensym "j" in
      let l = gensym "l" in
      Lam
        ( k,
          App
            (Var k, Lam (x, Lam (j, App (cps3 m, Lam (l, App (Var j, Var l))))))
        )
  | App (m, n) ->
      let k = gensym "k" in
      let vm = gensym "v" in
      let vn = gensym "v" in
      let a = gensym "a" in
      Lam
        ( k,
          App
            ( cps3 m,
              Lam
                ( vm,
                  App
                    ( cps3 n,
                      Lam
                        ( vn,
                          App (App (Var vm, Var vn), Lam (a, App (Var k, Var a)))
                        ) ) ) ) )

(* use higher order function in host language *)
let rec cps4 (e : expr) : (expr -> expr) -> expr =
  match e with
  | Var _ -> fun k -> k e
  | Lam (x, m) ->
      let j = gensym "j" in
      fun k -> k (Lam (x, Lam (j, (cps4 m) (fun l -> App (Var j, l)))))
  | App (m, n) ->
      let a = gensym "a" in
      fun k ->
        (cps4 m) (fun vm ->
            (cps4 n) (fun vn -> App (App (vm, vn), Lam (a, k (Var a)))))

(* uncurry, get Nielson & Nielson's one pass cps *)
let rec cps (e : expr) (k : expr -> expr) : expr =
  match e with
  | Var _ -> k e
  | Lam (x, m) ->
      let j = gensym "j" in
      k (Lam (x, Lam (j, cps m (fun l -> App (Var j, l)))))
  | App (m, n) ->
      let a = gensym "a" in
      cps m (fun vm -> cps n (fun vn -> App (App (vm, vn), Lam (a, k (Var a)))))

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
    {| λf.λj1.(j1 λx.λj2.(j2 λy.λj3.((f y) λa5.((a5 x) λa4.(j3 a4))))) |}]

let%expect_test _ =
  reset ();
  let e = Lam ("f", App (Var "f", Var "x")) in
  string_of_expr e |> print_endline;
  [%expect {| λf.(f x) |}];
  let t = cps e id in
  string_of_expr t |> print_endline;
  [%expect {| λf.λj1.((f x) λa2.(j1 a2)) |}]
