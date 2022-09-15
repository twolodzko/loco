open Parser

let%test _ = is_whitespace ' ' = true
let%test _ = is_whitespace '\t' = true
let%test _ = is_whitespace '1' = false

let%test _ =
  let s = Reader.of_string "   123 " in
  (
    skip_whitespace s;
    Stream.peek s
  )
  = Some '1'

let%test _ =
  let s = Reader.of_string "     " in
  (
    skip_whitespace s;
    Stream.peek s
  )
  = None

let%test _ = read_sexpr (Reader.of_string "123") = Integer 123
let%test _ = read_sexpr (Reader.of_string "-5") = Integer (-5)
let%test _ = read_sexpr (Reader.of_string "#t") = Bool true
let%test _ = read_sexpr (Reader.of_string "#f") = Bool false
let%test _ = read_sexpr (Reader.of_string "foo") = Symbol "foo"
let%test _ = read_sexpr (Reader.of_string "()") = List []
let%test _ = read_sexpr (Reader.of_string "(1 2 (3 4 ()))") = List [ Integer 1; Integer 2; List [ Integer 3; Integer 4; List [] ] ]
let%test _ = read_sexpr (Reader.of_string "(+)") = List [ Symbol "+" ]
let%test _ = read_sexpr (Reader.of_string "(- 2 2)") = List [ Symbol "-"; Integer 2; Integer 2 ]
let%test _ = read_sexpr (Reader.of_string "'()") = Quote (List [])
let%test _ = read_sexpr (Reader.of_string "'x") = Quote (Symbol "x")
let%test _ = read_sexpr (Reader.of_string "'#t") = Quote (Bool true)
let%test _ = read_sexpr (Reader.of_string "'(())") = Quote (List [ List [] ])
let%test _ = read_sexpr (Reader.of_string "\"abcde\"") = String "abcde"
let%test _ = read_sexpr (Reader.of_string "\"\"") = String ""
let%test _ = read_sexpr (Reader.of_string "\"123\"") = String "123"
let%test _ = read_sexpr (Reader.of_string "\"()\"") = String "()"
let%test _ = read_sexpr (Reader.of_string "\"\\\"hello\\\"\"") = String "\"hello\""
