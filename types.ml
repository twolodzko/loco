
type sexpr =
  | Nil
  | Integer of int
  | Bool of bool
  | Symbol of string
  | String of string
  | Quote of sexpr
  | List of sexpr list
  | Fun of (sexpr list -> env -> sexpr)
and env = sexpr Env.t

let rec string_of_sexpr = function
  | Nil -> "<nil>"
  | Integer s -> Int.to_string s
  | Bool true -> "#t"
  | Bool false -> "#f"
  | Symbol s -> s
  | String s -> Printf.sprintf "\"%s\"" s
  | Quote s -> "'" ^ string_of_sexpr s
  | List s -> s |>
      List.map string_of_sexpr |>
      String.concat " " |>
      fun x -> Printf.sprintf "(%s)" x
  | Fun _ -> "<fun>"

let print_sexpr s =
  print_string (string_of_sexpr s)

let is_true = function
  | Bool false -> false
  | _ -> true
