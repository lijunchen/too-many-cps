let gensym, reset =
  let counter = ref 0 in
  let gensym name =
    incr counter;
    name ^ "_" ^ string_of_int !counter
  in
  let reset () = counter := 0 in
  (gensym, reset)

type var = string [@@deriving show { with_path = false }]
type cvar = string [@@deriving show { with_path = false }]

type expr =
  | Var of var
  | App of expr * expr
  | Lam of var * expr
  | Pair of expr * expr
  | Proj of int * expr
  | Unit
  | Inj of int * expr
  | Let of var * expr * expr
  | Case of expr * var * expr * var * expr
  | BinOp of string * expr * expr
  | Print of expr
[@@deriving show { with_path = false }]

type value =
  | Unit
  | Pair of var * var
  | Inj of int * var
  | Lam of cvar * var * ctm
  | Print of var

and ctm =
  | LetVal of var * value * ctm
  | LetProj of var * int * var * ctm
  | LetCont of cvar * var * ctm * ctm
  | LetBinOp of var * string * var * var * ctm
  | AppCont of cvar * var
  | App of var * cvar * var
  | Case of var * cvar * cvar
[@@deriving show { with_path = false }]

let rec cps (e : expr) (k : var -> ctm) : ctm =
  match e with
  | Var x -> k x
  | Unit ->
      let x = gensym "x" in
      LetVal (x, Unit, k x)
  | App (e1, e2) ->
      let cont = gensym "$k" in
      let x = gensym "x" in
      cps e1 (fun z1 ->
          cps e2 (fun z2 -> LetCont (cont, x, k x, App (z1, cont, z2))))
  | Pair (e1, e2) ->
      let x = gensym "x" in
      cps e1 (fun z1 -> cps e2 (fun v2 -> LetVal (x, Pair (z1, v2), k x)))
  | Inj (i, e) ->
      let x = gensym "x" in
      cps e (fun z -> LetVal (x, Inj (i, z), k x))
  | Proj (i, e) ->
      let x = gensym "x" in
      cps e (fun z -> LetProj (x, i, z, k x))
  | Lam (x, e) ->
      let cont = gensym "$k" in
      let f = gensym "f" in
      LetVal (f, Lam (cont, x, cps e (fun z -> AppCont (cont, z))), k f)
  | Let (name, value, body) ->
      let cont = gensym "$k" in
      LetCont (cont, name, cps body k, cps value (fun z -> AppCont (cont, z)))
  | Case (e, x1, e1, x2, e2) ->
      let cont1 = gensym "$k" in
      let cont2 = gensym "$k" in
      cps e (fun z ->
          LetCont
            ( cont1,
              x1,
              cps e1 k,
              LetCont (cont2, x2, cps e2 k, Case (z, cont1, cont2)) ))
  | BinOp (op, lhs, rhs) ->
      let x = gensym "x" in
      cps lhs (fun z1 -> cps rhs (fun z2 -> LetBinOp (x, op, z1, z2, k x)))
  | Print e ->
      let x = gensym "x" in
      cps e (fun z -> LetVal (x, Print z, k x))

let%expect_test _ =
  (* let x = 1 + 2 in let y = 3 + 4 in print(x + y) *)
  reset ();
  let e =
    Let
      ( "x",
        BinOp ("+", Var "1", Var "2"),
        Let
          ( "y",
            BinOp ("+", Var "3", Var "4"),
            Print (BinOp ("+", Var "x", Var "y")) ) )
  in
  let c = cps e (fun k -> AppCont ("halt", k)) in
  show_ctm c |> print_endline;
  [%expect
    {|
    (LetCont ("$k_1", "x",
       (LetCont ("$k_3", "y",
          (LetBinOp ("x_6", "+", "x", "y",
             (LetVal ("x_5", (Print "x_6"), (AppCont ("halt", "x_5")))))),
          (LetBinOp ("x_4", "+", "3", "4", (AppCont ("$k_3", "x_4")))))),
       (LetBinOp ("x_2", "+", "1", "2", (AppCont ("$k_1", "x_2"))))))
    |}]

let%expect_test _ =
  (* fn x => f (x, y) *)
  reset ();
  let e : expr = Lam ("x", App (Var "f", Pair (Var "x", Var "y"))) in
  let c = cps e (fun k -> AppCont ("halt", k)) in
  show_ctm c |> print_endline;
  [%expect
    {|
    (LetVal ("f_2",
       (Lam ("$k_1", "x",
          (LetVal ("x_5", (Pair ("x", "y")),
             (LetCont ("$k_3", "x_4", (AppCont ("$k_1", "x_4")),
                (App ("f", "$k_3", "x_5"))))
             ))
          )),
       (AppCont ("halt", "f_2"))))
    |}]
