open Types

exception Not_callable

let rec eval sexpr env =
  match sexpr with
  | Quote s -> s
  | Symbol s -> Env.get s env
  | List l -> eval_list l env
  | _ -> sexpr

and eval_list l env =
  match l with
  | h :: args  -> (
      match (eval h env) with
      | Fun f -> f args env
      | _ -> raise Not_callable
    )
  | [] -> List []

let eval_all sexprs env =
  let rec impl ss env acc =
    match ss with
    | s :: tail ->
        let v = eval s env in
        impl tail env (v :: acc)
    |  [] -> List.rev acc
  in impl sexprs env []

let eval_stream stm env =
  let rec impl prev =
    try
      let inp = Parser.read_sexpr stm in
      let out = eval inp env in
        impl out
    with Stream.Failure -> prev
  in impl Nil

let eval_file path env =
  eval_stream (Reader.of_channel (open_in path)) env
