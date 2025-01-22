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

let rec m e =
  match e with
  | Var v -> AVar v
  | Lam (v, body) ->
      let cont = gensym "$k" in
      ALam ([ v; cont ], t body (fun rv -> CApp (AVar cont, [ rv ])))
  | App _ -> assert false

(*
  read as:
  the transformation of e into CPS, such that k will be invoked on its result
*)
and t (e : expr) (k : aexpr -> cexpr) =
  match e with
  | Var _ | Lam _ -> k (m e)
  | App (f, e) ->
      let rv = gensym "$rv" in
      let cont = ALam ([ rv ], k (AVar rv)) in
      t f (fun vf -> t e (fun ve -> CApp (vf, [ ve; cont ])))

let%expect_test _ =
  reset ();
  let e = App (Var "g", Var "a") in
  let c = t e (fun ans -> CApp (AVar "halt", [ ans ])) in
  print_endline (show_cexpr c);
  [%expect
    {|
    (CApp ((AVar "g"),
       [(AVar "a"); (ALam (["$rv_1"], (CApp ((AVar "halt"), [(AVar "$rv_1")]))))]
       ))
    |}]
