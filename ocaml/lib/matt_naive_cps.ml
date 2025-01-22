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
      ALam ([ v; cont ], t body (AVar cont))
  | App _ -> assert false

(*
  read as:
  the transformation of e into CPS, such that k will be invoked on its result
*)
and t (e : expr) (k : aexpr) =
  match e with
  | Var _ | Lam _ -> CApp (k, [ m e ])
  | App (f, e) ->
      let vf = gensym "$f" in
      let ve = gensym "$e" in
      t f (ALam ([ vf ], t e (ALam ([ ve ], CApp (AVar vf, [ AVar ve; k ])))))

let%expect_test _ =
  reset ();
  let e = App (Var "g", Var "a") in
  let k = AVar "halt" in
  let c = t e k in
  print_endline (show_cexpr c);
  [%expect
    {|
    (CApp (
       (ALam (["$f_1"],
          (CApp (
             (ALam (["$e_2"],
                (CApp ((AVar "$f_1"), [(AVar "$e_2"); (AVar "halt")])))),
             [(AVar "a")]))
          )),
       [(AVar "g")]))
    |}]
