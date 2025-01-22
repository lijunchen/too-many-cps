let gensym, reset =
  let counter = ref 0 in
  let gensym name =
    incr counter;
    name ^ "_" ^ string_of_int !counter
  in
  let reset () = counter := 0 in
  (gensym, reset)

type var = string [@@deriving show { with_path = false }]

type atom =
  | Lam of var list * expr
  | Var of var
  | Bool of bool
  | Num of int
  | Str of string
  | Unit
  | CallEC
  | CallCC

and expr =
  | Atom of atom
  | Begin of expr list
  | If of expr * expr * expr
  | Set of var * expr
  | Letrec of (var * atom) list * expr
  | Prim of prim * expr list
  | Call of expr * expr list

and prim = Add | Sub | Mul | Div | Eq [@@deriving show { with_path = false }]

type aexp =
  | ALam of var list * cexp
  | AVar of var
  | ABool of bool
  | ANum of int
  | AStr of string
  | AUnit

and cexp =
  | CIf of aexp * cexp * cexp
  | CSetThen of var * aexp * cexp
  | CLet of var * aexp * cexp
  | CLetrec of (var * aexp) list * cexp
  | CPrim of prim * aexp list
  | CCall of aexp * aexp list
[@@deriving show { with_path = false }]

let rec t_c (e : expr) (c : aexp) : cexp =
  match e with
  | Atom a -> CCall (c, [ m a ])
  | Begin [] -> CCall (c, [ AUnit ])
  | Begin [ e ] -> t_c e c
  | Begin (x :: xs) -> t_k x (fun _ -> t_c (Begin xs) c)
  | If (cond, ifso, ifnot) ->
      let k = gensym "$k" in
      CCall
        ( ALam
            ( [ k ],
              t_k cond (fun aexp ->
                  CIf (aexp, t_c ifso (AVar k), t_c ifnot (AVar k))) ),
          [ c ] )
  | Set (v, e) -> t_k e (fun aexp -> CSetThen (v, aexp, CCall (c, [ AUnit ])))
  | Letrec (bindings, body) ->
      CLetrec (List.map (fun (v, a) -> (v, m a)) bindings, t_c body c)
  | Prim (p, es) -> t_list_k es (fun aes -> CPrim (p, aes @ [ c ]))
  | Call (f, es) ->
      t_k f (fun af -> t_list_k es (fun aes -> CCall (af, aes @ [ c ])))

and t_k (e : expr) (k : aexp -> cexp) : cexp =
  match e with
  | Atom atom -> k (m atom)
  | Begin [] -> assert false
  | Begin [ expr ] -> t_k expr k
  | Begin (e :: es) -> t_k e (fun _ -> t_k (Begin es) k)
  | If (cond, t_branch, f_branch) ->
      let rv = gensym "$rv" in
      let cont = ALam ([ rv ], k (AVar rv)) in
      t_k cond (fun a_cond ->
          CIf (a_cond, t_c t_branch cont, t_c f_branch cont))
  | Set (var, expr) -> t_k expr (fun a -> CSetThen (var, a, k AUnit))
  | Letrec (binds, body) ->
      let new_binds = List.map (fun (v, a) -> (v, m a)) binds in
      CLetrec (new_binds, t_k body k)
  | Prim _ | Call _ ->
      let rv = gensym "$rv" in
      let k = ALam ([ rv ], k (AVar rv)) in
      t_c e k

and t_list_k (es : expr list) (k : aexp list -> cexp) : cexp =
  match es with
  | [] -> k []
  | x :: xs -> t_k x (fun hd -> t_list_k xs (fun tl -> k (hd :: tl)))

and m (e : atom) : aexp =
  match e with
  | Lam (vars, body) ->
      let k = gensym "$k" in
      ALam (vars @ [ k ], t_c body (AVar k))
  | Var v -> AVar v
  | Bool b -> ABool b
  | Num n -> ANum n
  | Str s -> AStr s
  | Unit -> AUnit
  | CallEC | CallCC ->
      ALam
        ( [ "f"; "cc" ],
          CCall
            ( AVar "f",
              [
                ALam ([ "x"; "_" ], CCall (AVar "cc", [ AVar "x" ])); AVar "cc";
              ] ) )

let%expect_test _ =
  reset ();

  let e = Lam ([ "x" ], Atom (Var "x")) in
  let mc = m e in
  show_aexp mc |> print_string;
  [%expect {| (ALam (["x"; "$k_1"], (CCall ((AVar "$k_1"), [(AVar "x")])))) |}];

  let e = Call (Atom (Var "g"), [ Atom (Var "a") ]) in
  let x = t_c e (AVar "halt") in
  show_cexp x |> print_string;
  [%expect {| (CCall ((AVar "g"), [(AVar "a"); (AVar "halt")])) |}];

  let e =
    Letrec
      ( [
          ( "f",
            Lam
              ( [ "n" ],
                If
                  ( Prim (Eq, [ Atom (Var "n"); Atom (Num 0) ]),
                    (* if n = 0 *)
                    Atom (Num 1),
                    (* then 1 *)
                    Prim
                      ( Mul,
                        [
                          (* else n * (f (n - 1)) *)
                          Atom (Var "n");
                          Call
                            ( Atom (Var "f"),
                              [ Prim (Sub, [ Atom (Var "n"); Atom (Num 1) ]) ]
                            );
                        ] ) ) ) );
        ],
        Call (Atom (Var "f"), [ Atom (Num 5) ]) (* Call f(5) *) )
  in

  let c = t_c e (AVar "halt") in
  show_cexp c |> print_string;
  [%expect
    {|
    (CLetrec (
       [("f",
         (ALam (["n"; "$k_2"],
            (CCall (
               (ALam (["$k_3"],
                  (CPrim (Eq,
                     [(AVar "n"); (ANum 0);
                       (ALam (["$rv_4"],
                          (CIf ((AVar "$rv_4"),
                             (CCall ((AVar "$k_3"), [(ANum 1)])),
                             (CPrim (Sub,
                                [(AVar "n"); (ANum 1);
                                  (ALam (["$rv_6"],
                                     (CCall ((AVar "f"),
                                        [(AVar "$rv_6");
                                          (ALam (["$rv_5"],
                                             (CPrim (Mul,
                                                [(AVar "n"); (AVar "$rv_5");
                                                  (AVar "$k_3")]
                                                ))
                                             ))
                                          ]
                                        ))
                                     ))
                                  ]
                                ))
                             ))
                          ))
                       ]
                     ))
                  )),
               [(AVar "$k_2")]))
            )))
         ],
       (CCall ((AVar "f"), [(ANum 5); (AVar "halt")]))))
    |}]

let%expect_test _ =
  reset ();
  (* (f (g x) (prim_add 1 2))  *)
  let e =
    Call
      ( Atom (Var "f"),
        [
          Call (Atom (Var "g"), [ Atom (Var "x") ]);
          Prim (Add, [ Atom (Num 1); Atom (Num 2) ]);
        ] )
  in
  let te = t_c e (AVar "halt") in
  show_cexp te |> print_string;
  [%expect
    {|
    (CCall ((AVar "g"),
       [(AVar "x");
         (ALam (["$rv_1"],
            (CPrim (Add,
               [(ANum 1); (ANum 2);
                 (ALam (["$rv_2"],
                    (CCall ((AVar "f"),
                       [(AVar "$rv_1"); (AVar "$rv_2"); (AVar "halt")]))
                    ))
                 ]
               ))
            ))
         ]
       ))
    |}]

let%expect_test _ =
  reset ();
  let _ =
    CCall
      ( AVar "g",
        [
          AVar "x";
          ALam
            ( [ "$rv_1" ],
              CPrim
                ( Add,
                  [
                    ANum 1;
                    ANum 2;
                    ALam
                      ( [ "$rv_2" ],
                        CCall
                          (AVar "f", [ AVar "$rv_1"; AVar "$rv_2"; AVar "halt" ])
                      );
                  ] ) );
        ] )
  in
  let g x k = k (x + 1) in
  let x = 10 in
  let f x y k = k (x + y) in

  let halt rv = print_int rv in

  g x (fun rv_1 ->
      let rv_2 = 1 + 2 in
      f rv_1 rv_2 halt);
  [%expect {| 14 |}]
