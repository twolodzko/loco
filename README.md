# Lisp implemented in OCaml, Oooh!

> *Do It, Do It Again, and Again, and Again ...*  
> &emsp; &mdash; *The Little Schemer* by Friedmann and Felleisen

![Lisp cycles XKCD #297: "Those are your father's parentheses. Elegant weapons for a more... civilized age."](https://imgs.xkcd.com/comics/lisp_cycles.png)

(source <https://xkcd.com/297/>)

**loco** is a minimal Scheme implementation in OCaml. Implementing lisp in a functional programming language like OCaml
is trivial as linked lists already are the basic building blocks and recursion is the default mode of execution.

The available data types are

```ocaml
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
```

They are evaluated as follows

```ocaml
let rec eval sexpr env =
  match sexpr with
  | Quote s -> s
  | Symbol s -> Env.get s env
  | List l -> eval_list l env
  | _ -> sexpr
```

and the environment `env` is defined as a list of pointers to maps

```ocaml
module LocalEnv = Map.Make(String)

type 'a t = 'a LocalEnv.t ref list
```

Lists are evaluated recursively, where the first element needs to be a function or a symbol naming the function
available in the enclosing environment, and the following arguments are treated as arguments of the function.

```ocaml
and eval_list l env =
  match l with
  | h :: args  -> (
      match (eval h env) with
      | Fun f -> f args env
      | _ -> raise Not_callable
    )
  | [] -> List []
```

This makes implementing the lisp functions straightforward. For example, `car` takes a single argument, a list, and
extracts its first element. All it needs is OCamls pattern matching and list operations.

```ocaml
let unpack_list = function
  | List s -> s
  | _ -> raise Not_a_list

let carf args _ =
  match unpack_list (List.hd args) with
  | h :: _ -> h
  | [] -> raise Invalid_input
```

For a more complicated example, `lambda` creates an anonymous function that evaluates its body in the local environment.

```ocaml
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
```

If you'd like more details, there is [nice series of detailed blog posts](https://bernsteinbear.com/blog/lisp/00_fundamentals/)
describing a similar project in OCaml.
