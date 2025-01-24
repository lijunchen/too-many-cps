let gensym, reset =
  let counter = ref 0 in
  let gensym name =
    incr counter;
    name ^ "" ^ string_of_int !counter
  in
  let reset () = counter := 0 in
  (gensym, reset)

type var = string [@@deriving show { with_path = false }]

type expr =
  | Int of int
  | Var of var
  | Prim of string * expr list
  | Lam of var list * expr
  | App of expr * expr list
  | Let of var * expr * expr
  | If of expr * expr * expr
[@@deriving show { with_path = false }]

type immexpr = ImmInt of int | ImmVar of var | ImmLam of var list * aexpr
[@@deriving show { with_path = false }]

and cexpr =
  | CImm of immexpr
  | CPrim of string * immexpr list
  | CApp of immexpr * immexpr list
  | CIf of immexpr * aexpr * aexpr

and aexpr = ACExpr of cexpr | ALet of var * cexpr * aexpr
[@@deriving show { with_path = false }]

let aexpr_of_cexpr (c : cexpr) : aexpr = ACExpr c

let rec normalize_term (e : expr) = normalize e aexpr_of_cexpr

and normalize (e : expr) (k : cexpr -> aexpr) : aexpr =
  match e with
  | Int x -> k (CImm (ImmInt x))
  | Var x -> k (CImm (ImmVar x))
  | Lam (params, body) -> k (CImm (ImmLam (params, normalize_term body)))
  | Let (x, e, body) -> normalize e (fun ve -> ALet (x, ve, normalize body k))
  | If (cond, e1, e2) ->
      normalize_name cond (fun vc ->
          k (CIf (vc, normalize_term e1, normalize_term e2)))
  | Prim (op, args) -> normalize_names args (fun vargs -> k (CPrim (op, vargs)))
  | App (f, args) ->
      normalize_name f (fun vf ->
          normalize_names args (fun vargs -> k (CApp (vf, vargs))))

and normalize_name (e : expr) (k : immexpr -> aexpr) : aexpr =
  match e with
  | Int x -> k (ImmInt x)
  | Var x -> k (ImmVar x)
  | _ ->
      let name = gensym "t" in
      normalize e (fun ve -> ALet (name, ve, k (ImmVar name)))

and normalize_names (es : expr list) (k : immexpr list -> aexpr) : aexpr =
  match es with
  | [] -> k []
  | x :: xs ->
      normalize_name x (fun vx -> normalize_names xs (fun vxs -> k (vx :: vxs)))

let%expect_test "001" =
  reset ();
  let e =
    Prim
      ( "+",
        [
          Prim ("+", [ Int 2; Int 2 ]);
          Let ("x", Int 1, App (Var "f", [ Var "x" ]));
        ] )
  in
  let a = normalize_term e in
  show_aexpr a |> print_endline;
  [%expect
    {|
    (ALet ("t1", (CPrim ("+", [(ImmInt 2); (ImmInt 2)])),
       (ALet ("x", (CImm (ImmInt 1)),
          (ALet ("t2", (CApp ((ImmVar "f"), [(ImmVar "x")])),
             (ACExpr (CPrim ("+", [(ImmVar "t1"); (ImmVar "t2")])))))
          ))
       ))
    |}]

let%expect_test "002" =
  reset ();
  let e = Prim ("add1", [ Let ("x", App (Var "f", [ Int 5 ]), Int 0) ]) in
  let a = normalize_term e in
  show_aexpr a |> print_endline;
  [%expect
    {|
    (ALet ("x", (CApp ((ImmVar "f"), [(ImmInt 5)])),
       (ALet ("t1", (CImm (ImmInt 0)), (ACExpr (CPrim ("add1", [(ImmVar "t1")])))
          ))
       ))
    |}]

let%expect_test "003" =
  reset ();
  let e =
    App (App (Var "f", [ Var "g" ]), [ App (Var "h", [ Var "x" ]); Int 3 ])
  in
  let a = normalize_term e in
  show_aexpr a |> print_endline;
  [%expect
    {|
    (ALet ("t1", (CApp ((ImmVar "f"), [(ImmVar "g")])),
       (ALet ("t2", (CApp ((ImmVar "h"), [(ImmVar "x")])),
          (ACExpr (CApp ((ImmVar "t1"), [(ImmVar "t2"); (ImmInt 3)])))))
       ))
    |}]

let%expect_test "004" =
  reset ();
  let e =
    Let
      ( "f",
        Lam
          ( [ "n" ],
            If
              ( Prim ("=", [ Var "n"; Int 0 ]),
                Int 1,
                Prim
                  ( "*",
                    [
                      Var "n"; App (Var "f", [ Prim ("-", [ Var "n"; Int 1 ]) ]);
                    ] ) ) ),
        App (Var "f", [ Int 20 ]) )
  in
  let a = normalize_term e in
  show_aexpr a |> print_endline;
  [%expect
    {|
    (ALet ("f",
       (CImm
          (ImmLam (["n"],
             (ALet ("t1", (CPrim ("=", [(ImmVar "n"); (ImmInt 0)])),
                (ACExpr
                   (CIf ((ImmVar "t1"), (ACExpr (CImm (ImmInt 1))),
                      (ALet ("t3", (CPrim ("-", [(ImmVar "n"); (ImmInt 1)])),
                         (ALet ("t2", (CApp ((ImmVar "f"), [(ImmVar "t3")])),
                            (ACExpr (CPrim ("*", [(ImmVar "n"); (ImmVar "t2")])))
                            ))
                         ))
                      )))
                ))
             ))),
       (ACExpr (CApp ((ImmVar "f"), [(ImmInt 20)])))))
    |}]

let%expect_test "005" =
  reset ();
  let e =
    Prim ("*", [ Prim ("+", [ Int 1; Int 2 ]); Prim ("+", [ Int 3; Int 4 ]) ])
  in
  let a = normalize_term e in
  show_aexpr a |> print_endline;
  [%expect
    {|
    (ALet ("t1", (CPrim ("+", [(ImmInt 1); (ImmInt 2)])),
       (ALet ("t2", (CPrim ("+", [(ImmInt 3); (ImmInt 4)])),
          (ACExpr (CPrim ("*", [(ImmVar "t1"); (ImmVar "t2")])))))
       ))
    |}]

let%expect_test "006" =
  reset ();
  let e =
    Let
      ( "x",
        If
          ( If
              ( Let ("a", Int 1, Prim ("<", [ Var "a"; Int 2 ])),
                Prim ("+", [ Int 3; Int 4 ]),
                Prim ("+", [ Int 5; Int 6 ]) ),
            If
              ( Let ("a", Int 7, Prim ("<", [ Var "a"; Int 8 ])),
                Prim ("+", [ Int 9; Int 10 ]),
                Prim ("+", [ Int 11; Int 12 ]) ),
            If
              ( Let ("a", Int 13, Prim ("<", [ Var "a"; Int 14 ])),
                Prim ("+", [ Int 15; Int 16 ]),
                Prim ("+", [ Int 17; Int 18 ]) ) ),
        Prim ("+", [ Var "x"; Int 1 ]) )
  in
  let a = normalize_term e in
  show_aexpr a |> print_endline;
  [%expect
    {|
    (ALet ("a", (CImm (ImmInt 1)),
       (ALet ("t2", (CPrim ("<", [(ImmVar "a"); (ImmInt 2)])),
          (ALet ("t1",
             (CIf ((ImmVar "t2"),
                (ACExpr (CPrim ("+", [(ImmInt 3); (ImmInt 4)]))),
                (ACExpr (CPrim ("+", [(ImmInt 5); (ImmInt 6)]))))),
             (ALet ("x",
                (CIf ((ImmVar "t1"),
                   (ALet ("a", (CImm (ImmInt 7)),
                      (ALet ("t4", (CPrim ("<", [(ImmVar "a"); (ImmInt 8)])),
                         (ACExpr
                            (CIf ((ImmVar "t4"),
                               (ACExpr (CPrim ("+", [(ImmInt 9); (ImmInt 10)]))),
                               (ACExpr (CPrim ("+", [(ImmInt 11); (ImmInt 12)])))
                               )))
                         ))
                      )),
                   (ALet ("a", (CImm (ImmInt 13)),
                      (ALet ("t3", (CPrim ("<", [(ImmVar "a"); (ImmInt 14)])),
                         (ACExpr
                            (CIf ((ImmVar "t3"),
                               (ACExpr (CPrim ("+", [(ImmInt 15); (ImmInt 16)]))),
                               (ACExpr (CPrim ("+", [(ImmInt 17); (ImmInt 18)])))
                               )))
                         ))
                      ))
                   )),
                (ACExpr (CPrim ("+", [(ImmVar "x"); (ImmInt 1)])))))
             ))
          ))
       ))
    |}]
