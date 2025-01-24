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

let is_value (e : expr) : bool =
  match e with Int _ | Var _ | Lam _ -> true | _ -> false

let id x = x

let rec normalize_term (e : expr) : expr = normalize e id

and normalize (e : expr) (k : expr -> expr) : expr =
  match e with
  | Int _ | Var _ -> k e
  | Lam (params, body) -> k (Lam (params, normalize_term body))
  | Let (name, value, body) ->
      normalize value (fun ve -> Let (name, ve, normalize body k))
  | If (cond, m1, m2) ->
      normalize_name cond (fun vc ->
          k (If (vc, normalize_term m1, normalize_term m2)))
  | Prim (op, args) -> normalize_names args (fun vargs -> k (Prim (op, vargs)))
  | App (f, args) ->
      normalize_name f (fun vf ->
          normalize_names args (fun vargs -> k (App (vf, vargs))))

and normalize_name (e : expr) (k : expr -> expr) : expr =
  normalize e (fun ve ->
      if is_value ve then k ve
      else
        let t = gensym "t" in
        Let (t, ve, k (Var t)))

and normalize_names (es : expr list) (k : expr list -> expr) : expr =
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
  show_expr a |> print_endline;
  [%expect
    {|
    (Let ("t1", (Prim ("+", [(Int 2); (Int 2)])),
       (Let ("x", (Int 1),
          (Let ("t2", (App ((Var "f"), [(Var "x")])),
             (Prim ("+", [(Var "t1"); (Var "t2")]))))
          ))
       ))
    |}]

let%expect_test "002" =
  reset ();
  let e = Prim ("add1", [ Let ("x", App (Var "f", [ Int 5 ]), Int 0) ]) in
  let a = normalize_term e in
  show_expr a |> print_endline;
  [%expect
    {| (Let ("x", (App ((Var "f"), [(Int 5)])), (Prim ("add1", [(Int 0)])))) |}]

let%expect_test "003" =
  reset ();
  let e =
    App (App (Var "f", [ Var "g" ]), [ App (Var "h", [ Var "x" ]); Int 3 ])
  in
  let a = normalize_term e in
  show_expr a |> print_endline;
  [%expect
    {|
    (Let ("t1", (App ((Var "f"), [(Var "g")])),
       (Let ("t2", (App ((Var "h"), [(Var "x")])),
          (App ((Var "t1"), [(Var "t2"); (Int 3)]))))
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
  show_expr a |> print_endline;
  [%expect
    {|
    (Let ("f",
       (Lam (["n"],
          (Let ("t1", (Prim ("=", [(Var "n"); (Int 0)])),
             (If ((Var "t1"), (Int 1),
                (Let ("t2", (Prim ("-", [(Var "n"); (Int 1)])),
                   (Let ("t3", (App ((Var "f"), [(Var "t2")])),
                      (Prim ("*", [(Var "n"); (Var "t3")]))))
                   ))
                ))
             ))
          )),
       (App ((Var "f"), [(Int 20)]))))
    |}]

let%expect_test "005" =
  reset ();
  let e =
    Prim ("*", [ Prim ("+", [ Int 1; Int 2 ]); Prim ("+", [ Int 3; Int 4 ]) ])
  in
  let a = normalize_term e in
  show_expr a |> print_endline;
  [%expect
    {|
    (Let ("t1", (Prim ("+", [(Int 1); (Int 2)])),
       (Let ("t2", (Prim ("+", [(Int 3); (Int 4)])),
          (Prim ("*", [(Var "t1"); (Var "t2")]))))
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
  show_expr a |> print_endline;
  [%expect
    {|
    (Let ("a", (Int 1),
       (Let ("t1", (Prim ("<", [(Var "a"); (Int 2)])),
          (Let ("t2",
             (If ((Var "t1"), (Prim ("+", [(Int 3); (Int 4)])),
                (Prim ("+", [(Int 5); (Int 6)])))),
             (Let ("x",
                (If ((Var "t2"),
                   (Let ("a", (Int 7),
                      (Let ("t4", (Prim ("<", [(Var "a"); (Int 8)])),
                         (If ((Var "t4"), (Prim ("+", [(Int 9); (Int 10)])),
                            (Prim ("+", [(Int 11); (Int 12)]))))
                         ))
                      )),
                   (Let ("a", (Int 13),
                      (Let ("t3", (Prim ("<", [(Var "a"); (Int 14)])),
                         (If ((Var "t3"), (Prim ("+", [(Int 15); (Int 16)])),
                            (Prim ("+", [(Int 17); (Int 18)]))))
                         ))
                      ))
                   )),
                (Prim ("+", [(Var "x"); (Int 1)]))))
             ))
          ))
       ))
    |}]
