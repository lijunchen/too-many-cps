let genusym, ureset =
  let counter = ref 0 in
  let gensym name =
    incr counter;
    name ^ "_" ^ string_of_int !counter
  in
  let reset () = counter := 0 in
  (gensym, reset)

let genksym, kreset =
  let counter = ref 0 in
  let gensym name =
    incr counter;
    name ^ "_" ^ string_of_int !counter
  in
  let reset () = counter := 0 in
  (gensym, reset)

(* Lambda Calculus *)
type expr = Var of string | Lam of string * expr | App of expr * expr
[@@deriving show { with_path = false }]

type uvar = string [@@deriving show { with_path = false }]
type kvar = string [@@deriving show { with_path = false }]

type uexpr = ULam of uvar * kvar * call | UVar of uvar
and kexpr = Cont of kvar * call | KVar of kvar

and call = UCall of uexpr * uexpr * kexpr | KCall of kexpr * uexpr
[@@deriving show { with_path = false }]

let rec t_k (e : expr) k =
  match e with
  | Var _ | Lam _ -> k (m e)
  | App (f, e) ->
      let rv = genusym "$rv" in
      let cont = Cont (rv, k (UVar rv)) in
      t_k f (fun vf -> t_k e (fun ve -> UCall (vf, ve, cont)))

and t_c (e : expr) (c : kexpr) : call =
  match e with
  | Var _ | Lam _ -> KCall (c, m e)
  | App (f, e) -> t_k f (fun vf -> t_k e (fun ve -> UCall (vf, ve, c)))

and m (e : expr) : uexpr =
  match e with
  | Var x -> UVar x
  | Lam (v, body) ->
      let k = genksym "$k" in
      ULam (v, k, t_c body (KVar k))
  | _ -> assert false

let%expect_test _ =
  ureset ();
  kreset ();
  let e = App (Var "g", Var "a") in
  let c = t_c e (KVar "halt") in
  print_endline (show_call c);
  [%expect {| (UCall ((UVar "g"), (UVar "a"), (KVar "halt"))) |}]
