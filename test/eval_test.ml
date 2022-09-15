open Eval
open Types

let%test _ = let v = eval (Bool true) [] in v = Bool true
let%test _ = let v = eval (Integer 42) [] in v = Integer 42
let%test _ = let v = eval (List []) [] in v = List []
let%test _ = let v = eval (Quote (Symbol "foo")) [] in v = Symbol "foo"
let%test _ =
  let v = eval (Quote (List [Symbol "foo"; Symbol "bar"; Bool false])) [] in v =
  List [Symbol "foo"; Symbol "bar"; Bool false]
let%test _ = let v = eval (Symbol "x") (Env.of_list [("x", Integer 1)]) in v = Integer 1
let%test _ =
  try let _ = eval (Symbol "x") [] in false
  with Not_found -> true
let%test _ =
  try let _ = eval (List [Symbol "x"; Integer 42]) (Env.of_list [("x", Integer 1)]) in false
  with Not_callable -> true

let%test _ =
  let v = eval_all [Symbol "x"; Integer 42; Quote (Symbol "a")] (Env.of_list [("x", Bool true)])
  in v = [Bool true; Integer 42; Symbol "a"]

let%test _ = eval_stream (Reader.of_string "42") [] = Integer 42
let%test _ = eval_stream (Reader.of_string "'(1 2 a ())") [] = List [ Integer 1; Integer 2; Symbol "a"; List []]
