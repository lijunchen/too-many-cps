let gensym, reset =
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

(* CPS form *)
type aexpr = ALam of string list * cexpr | AVar of string
and cexpr = CApp of aexpr * aexpr list [@@deriving show { with_path = false }]

let rec t_k (e : expr) (k : aexpr -> cexpr) : cexpr =
  match e with
  | Var _ | Lam _ -> k (m e)
  | App (f, e) ->
      let rv = gensym "$rv" in
      let cont = ALam ([ rv ], k (AVar rv)) in
      t_k f (fun vf -> t_k e (fun ve -> CApp (vf, [ ve; cont ])))

and t_c (e : expr) (c : aexpr) : cexpr =
  match e with
  | Var _ | Lam _ -> CApp (c, [ m e ])
  | App (f, e) -> t_k f (fun vf -> t_k e (fun ve -> CApp (vf, [ ve; c ])))

and m (e : expr) : aexpr =
  match e with
  | Var v -> AVar v
  | Lam (v, body) ->
      let cont = gensym "$k" in
      ALam ([ v; cont ], t_c body (AVar cont))
  | App _ -> assert false

let%expect_test _ =
  reset ();
  let e = App (Var "g", Var "a") in
  let c = t_c e (AVar "halt") in
  print_endline (show_cexpr c);
  [%expect {| (CApp ((AVar "g"), [(AVar "a"); (AVar "halt")])) |}]
