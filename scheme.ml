open Types

exception Invalid_input
exception Not_a_list
exception Not_a_symbol
exception Wrong_number_of_args
exception Not_a_number

let (%>) f g =
  fun a e ->
    let a = f a e in g a e

let unpack_list = function
  | List s -> s
  | _ -> raise Not_a_list

let unpack_symbol = function
  | Symbol s -> s
  | _ -> raise Not_a_symbol

let last args _ =
  let rec impl = function
    | [x] -> x
    | _ :: t -> impl t
    | [] -> raise Wrong_number_of_args
  in impl args

let listf args _ = List args

let carf args _ =
  match unpack_list (List.hd args) with
  | h :: _ -> h
  | [] -> raise Invalid_input

let cdrf args _ =
  match unpack_list (List.hd args) with
  | _ :: t -> List t
  | [] -> raise Invalid_input

let consf args _ =
  match args with
  | h :: [(List l)] -> List (h :: l)
  | h :: [l] -> List (h :: [l])
  | _ -> raise Wrong_number_of_args

let quotef args _ =
  match args with
  | [h] -> h
  | _ -> raise Wrong_number_of_args

let definef args env =
  match args with
  | (Symbol k) :: [v] -> (
      let v = Eval.eval v env in
      let _ = Env.add k v env in v
    )
  | _ :: _ :: _ -> raise Not_a_symbol
  | _ -> raise Wrong_number_of_args

let notf args _ =
  match args with
  | [a] -> Bool (not (is_true a))
  | _ -> raise Wrong_number_of_args

let rec andf args env =
  match args with
  | h :: t -> (
      if not (is_true h) then Bool false
      else (andf t env)
    )
  | [] -> Bool true

let rec orf args env =
  match args with
  | h :: t -> (
      if (is_true h) then Bool true
      else (orf t env)
    )
  | [] -> Bool false

let evalf args env =
  match args with
  | [s] -> Eval.eval (Eval.eval s env) env
  | _ -> raise Wrong_number_of_args

let equalf args _ =
  let impl x y =
    match x, y with
    | (Fun x), (Fun y) -> Bool (x == y)
    | x, y -> (
        try
          if x = y then Bool true
          else Bool false
        with _ -> Bool false
      )
  in match args with
  | x :: [y] -> impl x y
  | h :: t -> List.fold_left impl h t
  | _ -> raise Wrong_number_of_args

let sumf x y = match x, y with
  | Integer x, Integer y -> Integer (x + y)
  | _ -> raise Not_a_number

let diff args _ =
  let impl x y = match x, y with
    | Integer x, Integer y -> Integer (x - y)
    | _ -> raise Not_a_number
  in match args with
  | [Integer x] -> Integer (-x)
  | (Integer h) :: t -> List.fold_left impl (Integer h) t
  | _ -> raise Not_a_number

let mulf x y = match x, y with
  | Integer x, Integer y -> Integer (x * y)
  | _ -> raise Not_a_number

let divf args _ =
  let impl x y = match x, y with
    | Integer x, Integer y -> Integer (x / y)
    | _ -> raise Not_a_number
  in match args with
  | [Integer _] -> Integer 0
  | (Integer h) :: t -> List.fold_left impl (Integer h) t
  | _ -> raise Not_a_number

let rec eqf args env =
  match args with
  | [] | [Integer _] -> Bool true
  | (Integer x) :: ((Integer y) :: _ as t) -> (
      if x = y then eqf t env
      else Bool false
    )
  | _ -> raise Not_a_number

let rec ltf args env =
  match args with
  | [] | [Integer _] -> Bool true
  | (Integer x) :: ((Integer y) :: _ as t) -> (
      if x < y then ltf t env
      else Bool false
    )
  | _ -> raise Not_a_number

let rec gtf args env =
  match args with
  | [] | [Integer _] -> Bool true
  | (Integer x) :: ((Integer y) :: _ as t) -> (
      if x > y then gtf t env
      else Bool false
    )
  | _ -> raise Not_a_number

let fold_left f init =
  fun args _ -> List.fold_left f init args

let is_number args _ =
  match args with
  | [Integer _] -> Bool true
  | [_] -> Bool false
  | _ -> raise Wrong_number_of_args

let is_bool args _ =
  match args with
  | [Bool _] -> Bool true
  | [_] -> Bool false
  | _ -> raise Wrong_number_of_args

let is_symbol args _ =
  match args with
  | [Symbol _] -> Bool true
  | [_] -> Bool false
  | _ -> raise Wrong_number_of_args

let is_string args _ =
  match args with
  | [String _] -> Bool true
  | [_] -> Bool false
  | _ -> raise Wrong_number_of_args

let is_pair args _ =
  match args with
  | [List _] -> Bool true
  | [_] -> Bool false
  | _ -> raise Wrong_number_of_args

let is_procedure args _ =
  match args with
  | [Fun _] -> Bool true
  | [_] -> Bool false
  | _ -> raise Wrong_number_of_args

let is_null args _ =
  match args with
  | [List []] -> Bool true
  | [_] -> Bool false
  | _ -> raise Wrong_number_of_args

let lambdaf args env =
  let rec init vars args parent local =
    match vars, args with
    | (Symbol k) :: tk, v :: tv -> (
        let v = Eval.eval v parent in
        let _ = Env.add k v local in
        init tk tv parent local
      )
    | [], [] -> ()
    | _, _ -> raise Wrong_number_of_args
  in match args with
  | List vars :: body ->
      Fun (fun a e ->
        let local = Env.of_env env in
        init vars a e local;
        let v = Eval.eval_all body local in
          last v e
      )
  | _ -> raise Invalid_input

let let_impl args local env =
  let rec bind = function
    | (List [(Symbol k); v]) :: tail -> (
        let v = Eval.eval v env in
          let _ =  Env.add k v local in
          bind tail
      )
    | [] -> ()
    | _ -> raise Invalid_input
  in match args with
  | (List bs) :: body -> (
      bind bs;
      let v = Eval.eval_all body local in
        last v env
    )
  | _ -> raise Invalid_input

let letf args env =
  let local = Env.of_env env in
    let_impl args local env

let letstarf args env =
  let local = Env.of_env env in
    let_impl args local local

let iff args env =
  match args with
  | c :: t :: [f] -> (
      let v = Eval.eval c env in
        if is_true v then Eval.eval t env
        else Eval.eval f env
    )
  | _ -> raise Invalid_input

let rec condf args env =
  match args with
  | (List [test; expr]) :: tail ->
      if is_true (Eval.eval test env) then
        Eval.eval expr env
      else condf tail env
  | [] -> Nil
  | _ -> raise Invalid_input

let unquoted_string = function
  | String s -> s
  | s -> string_of_sexpr s

let stringf args _ =
  String (String.concat " " (List.map unquoted_string args))

let errorf args _ =
  failwith (String.concat " " (List.map unquoted_string args))

let loadf args env =
  match args with
  | [String path] -> Eval.eval_file path env
  | _ -> raise Invalid_input

let setf args env =
  match args with
  | [Symbol k; v] ->
      let v = Eval.eval v env in
      let _ = Env.update k v env in v
  | _ -> raise Invalid_input

let procedures = Env.of_list [
  ("-", Fun (Eval.eval_all %> diff));
  ("*", Fun (Eval.eval_all %> fold_left mulf (Integer 1)));
  ("/", Fun (Eval.eval_all %> divf));
  ("+", Fun (Eval.eval_all %> fold_left sumf (Integer 0)));
  ("<", Fun (Eval.eval_all %> ltf));
  ("=", Fun (Eval.eval_all %> eqf));
  (">", Fun (Eval.eval_all %> gtf));
  ("and", Fun (Eval.eval_all %> andf));
  ("begin", Fun (Eval.eval_all %> last));
  ("bool?", Fun (Eval.eval_all %> is_bool));
  ("car", Fun (Eval.eval_all %> carf));
  ("cdr", Fun (Eval.eval_all %> cdrf));
  ("cond", Fun condf);
  ("cons", Fun (Eval.eval_all %> consf));
  ("define", Fun definef);
  ("else", Bool true);
  ("eq?", Fun (Eval.eval_all %> equalf));
  ("equal?", Fun (Eval.eval_all %> equalf));
  ("error", Fun (Eval.eval_all %> errorf));
  ("eval", Fun evalf);
  ("if", Fun iff);
  ("lambda", Fun lambdaf);
  ("let", Fun letf);
  ("let*", Fun letstarf);
  ("list", Fun (Eval.eval_all %> listf));
  ("load", Fun loadf);
  ("not", Fun (Eval.eval_all %> notf));
  ("null?", Fun (Eval.eval_all %> is_null));
  ("number?", Fun (Eval.eval_all %> is_number));
  ("or", Fun (Eval.eval_all %> orf));
  ("pair?", Fun (Eval.eval_all %> is_pair));
  ("procedure?", Fun (Eval.eval_all %> is_procedure));
  ("quote", Fun quotef);
  ("set!", Fun setf);
  ("string?", Fun (Eval.eval_all %> is_string));
  ("string", Fun (Eval.eval_all %> stringf));
  ("symbol?", Fun (Eval.eval_all %> is_symbol));
]
