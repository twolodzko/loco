open Types

let procedures = Env.of_env Scheme.procedures

let%test "(list 1 2)" =
  Eval.eval (List [ Symbol "list"; Integer 1; Integer 2 ]) procedures = List [ Integer 1; Integer 2 ]
let%test "(list)" =
  Eval.eval (List [ Symbol "list" ]) procedures = List []
let%test "(list 'a 2)" =
  Eval.eval (List [ Symbol "list"; Quote (Symbol "a"); Integer 2 ]) procedures = List [ Symbol "a"; Integer 2 ]

let%test "(car '(1))" =
  Eval.eval (List [ Symbol "car"; Quote (List [ Integer 1 ]) ]) procedures = Integer 1
let%test "(car '(1 2))" =
  Eval.eval (List [ Symbol "car"; Quote (List [ Integer 1; Integer 2 ]) ]) procedures = Integer 1

let%test "(cdr '(1 2 3))" =
  Eval.eval (List [ Symbol "cdr"; Quote (List [ Integer 1; Integer 2; Integer 3 ]) ]) procedures = List [ Integer 2; Integer 3 ]
let%test "(cdr '(1))" =
  Eval.eval (List [ Symbol "cdr"; Quote (List [ Integer 1 ]) ]) procedures = List []

let%test "(cons 1 '())" =
  Eval.eval (List [ Symbol "cons"; Integer 1; Quote (List []) ]) procedures = List [ Integer 1 ]
let%test "(cons 1 '(2 3))" =
  Eval.eval (List [ Symbol "cons"; Integer 1; Quote (List [ Integer 2; Integer 3 ]) ]) procedures = List [ Integer 1; Integer 2; Integer 3 ]

let%test "(quote a)" =
  Eval.eval (List [ Symbol "quote"; Symbol "a" ]) procedures = Symbol "a"
let%test "(quote ())" =
  Eval.eval (List [ Symbol "quote"; List [] ]) procedures = List []
let%test "(quote (1 2 3))" =
  Eval.eval (List [ Symbol "quote"; List [ Integer 1; Integer 2; Integer 3 ] ]) procedures = List [ Integer 1; Integer 2; Integer 3 ]

let%test "(not #t)" =
  Eval.eval (List [ Symbol "not"; Bool true ]) procedures = Bool false
let%test "(not #f)" =
  Eval.eval (List [ Symbol "not"; Bool false ]) procedures = Bool true
let%test "(not 0)" =
  Eval.eval (List [ Symbol "not"; Integer 0 ]) procedures = Bool false

let%test "(and)" =
  Eval.eval (List [ Symbol "and" ]) procedures = Bool true
let%test "(and #t)" =
  Eval.eval (List [ Symbol "and"; Bool true ]) procedures = Bool true
let%test "(and #t #t #t)" =
  Eval.eval (List [ Symbol "and"; Bool true; Bool true; Bool true ]) procedures = Bool true
let%test "(and #f)" =
  Eval.eval (List [ Symbol "and"; Bool false ]) procedures = Bool false
let%test "(and #t #t #f)" =
  Eval.eval (List [ Symbol "and"; Bool true; Bool true; Bool false ]) procedures = Bool false
let%test "(and 1 2 3)" =
  Eval.eval (List [ Symbol "and"; Integer 1; Integer 2; Integer 3 ]) procedures = Bool true

let%test "(or)" =
  Eval.eval (List [ Symbol "or" ]) procedures = Bool false
let%test "(or #f)" =
  Eval.eval (List [ Symbol "or"; Bool false ]) procedures = Bool false
let%test "(or #f #t)" =
  Eval.eval (List [ Symbol "or"; Bool false; Bool true ]) procedures = Bool true
let%test "(or #f #t #f)" =
  Eval.eval (List [ Symbol "or"; Bool false; Bool true; Bool false ]) procedures = Bool true
let%test "(or #f '())')" =
  Eval.eval (List [ Symbol "or"; Bool false; Quote (List []) ]) procedures = Bool true

let%test "(+)" =
  Eval.eval (List [ Symbol "+" ]) procedures = Integer 0
let%test "(+ 10)" =
  Eval.eval (List [ Symbol "+"; Integer 10 ]) procedures = Integer 10
let%test "(+ 10 5 2)" =
  Eval.eval (List [ Symbol "+"; Integer 10; Integer 5; Integer 2 ]) procedures = Integer 17
let%test "(+ 10 (+ 5 2))" =
  Eval.eval (List [ Symbol "+"; Integer 10; List [ Symbol "+"; Integer 5; Integer 2 ] ]) procedures = Integer 17

let%test "(- 10)" =
  Eval.eval (List [ Symbol "-"; Integer 10 ]) procedures = Integer (-10)
let%test "(- 10 5)" =
  Eval.eval (List [ Symbol "-"; Integer 10; Integer 5 ]) procedures = Integer 5
let%test "(- 10 5 2)" =
  Eval.eval (List [ Symbol "-"; Integer 10; Integer 5; Integer 2 ]) procedures = Integer 3
let%test "(- 10 (- 5 2))" =
  Eval.eval (List [ Symbol "-"; Integer 10; List [ Symbol "-"; Integer 5; Integer 2 ] ]) procedures = Integer 7
let%test "(- -10 2)" =
  Eval.eval (List [ Symbol "-"; Integer (-10); Integer 2 ]) procedures = Integer (-12)

let%test "(*)" =
  Eval.eval (List [ Symbol "*" ]) procedures = Integer 1
let%test "(* 10)" =
  Eval.eval (List [ Symbol "*"; Integer 10 ]) procedures = Integer 10
let%test "(* 10 2 3)" =
  Eval.eval (List [ Symbol "*"; Integer 10; Integer 2; Integer 3 ]) procedures = Integer 60
let%test "(* 10 -2)" =
  Eval.eval (List [ Symbol "*"; Integer 10; Integer (-2) ]) procedures = Integer (-20)

let%test "(/ 4 2)" =
  Eval.eval (List [ Symbol "/"; Integer 4; Integer 2 ]) procedures = Integer 2
let%test "(/ 20 5 2)" =
  Eval.eval (List [ Symbol "/"; Integer 20; Integer 5; Integer 2 ]) procedures = Integer 2

let%test "(=)" =
  Eval.eval (List [ Symbol "=" ]) procedures = Bool true
let%test "(= 1)" =
  Eval.eval (List [ Symbol "="; Integer 1 ]) procedures = Bool true
let%test "(= 1 1)" =
  Eval.eval (List [ Symbol "="; Integer 1; Integer 1 ]) procedures = Bool true
let%test "(= 1 1 1)" =
  Eval.eval (List [ Symbol "="; Integer 1; Integer 1; Integer 1 ]) procedures = Bool true
let%test "(= 1 1 2)" =
  Eval.eval (List [ Symbol "="; Integer 1; Integer 1; Integer 2 ]) procedures = Bool false

let%test "'(+ 2 2)" =
  Eval.eval (Quote ( List [ Symbol "+"; Integer 2; Integer 2 ] )) procedures = List [ Symbol "+"; Integer 2; Integer 2 ]
let%test "(eval '(+ 2 2))" =
  Eval.eval (List [ Symbol "eval"; Quote( List [ Symbol "+"; Integer 2; Integer 2 ]) ]) procedures = Integer 4

let%test "(<)" =
  Eval.eval (List [ Symbol "<" ]) procedures = Bool true
let%test "(< 1)" =
  Eval.eval (List [ Symbol "<"; Integer 1 ]) procedures = Bool true
let%test "(< 1 2)" =
  Eval.eval (List [ Symbol "<"; Integer 1; Integer 2 ]) procedures = Bool true
let%test "(< 1 2 3)" =
  Eval.eval (List [ Symbol "<"; Integer 1; Integer 2; Integer 3 ]) procedures = Bool true
let%test "(< 1 3 2)" =
  Eval.eval (List [ Symbol "<"; Integer 1; Integer 3; Integer 2 ]) procedures = Bool false

let%test "(>)" =
  Eval.eval (List [ Symbol ">" ]) procedures = Bool true
let%test "(> 3)" =
  Eval.eval (List [ Symbol ">"; Integer 3 ]) procedures = Bool true
let%test "(> 3 2)" =
  Eval.eval (List [ Symbol ">"; Integer 3; Integer 2 ]) procedures = Bool true
let%test "(> 3 2 1)" =
  Eval.eval (List [ Symbol ">"; Integer 3; Integer 2; Integer 1 ]) procedures = Bool true
let%test "(> 3 1 2)" =
  Eval.eval (List [ Symbol ">"; Integer 3; Integer 1; Integer 2 ]) procedures = Bool false

let%test "(equal? 2 2)" =
  Eval.eval (List [ Symbol "equal?"; Integer 2; Integer 2 ] ) procedures = Bool true
let%test "(equal? 1 2)" =
  Eval.eval (List [ Symbol "equal?"; Integer 1; Integer 2 ] ) procedures = Bool false
let%test "(equal? #t #t)" =
  Eval.eval (List [ Symbol "equal?"; Bool true; Bool true ] ) procedures = Bool true
let%test "(equal? #t #f)" =
  Eval.eval (List [ Symbol "equal?"; Bool true; Bool false ] ) procedures = Bool false
let%test "(equal? #f #f)" =
  Eval.eval (List [ Symbol "equal?"; Bool false; Bool false ] ) procedures = Bool true
let%test "(equal? '() '())" =
  Eval.eval (List [ Symbol "equal?"; Quote (List []); Quote (List []) ] ) procedures = Bool true
let%test "(equal? '() '(1 2))" =
  Eval.eval (List [ Symbol "equal?"; Quote (List []); Quote (List [ Integer 1; Integer 2 ]) ] ) procedures = Bool false
let%test "(equal? '(1 2) '(1 2))" =
  Eval.eval (List [ Symbol "equal?"; Quote (List [ Integer 1; Integer 2 ]); Quote (List [ Integer 1; Integer 2 ]) ] ) procedures = Bool true
let%test "(equal? 'a 'a)" =
  Eval.eval (List [ Symbol "equal?"; Quote (Symbol "a"); Quote (Symbol "a") ] ) procedures = Bool true
let%test "(equal? car car)" =
  Eval.eval (List [ Symbol "equal?"; Symbol "car"; Symbol "car" ] ) procedures = Bool true
let%test "(equal? car cdr)" =
  Eval.eval (List [ Symbol "equal?"; Symbol "car"; Symbol "cdr" ] ) procedures = Bool false
let%test "(equal? car 'car')" =
  Eval.eval (List [ Symbol "equal?"; Symbol "car"; Quote (Symbol "car") ] ) procedures = Bool false

let%test "(null? '())" =
  Eval.eval (List [ Symbol "null?"; Quote (List []) ]) procedures = Bool true
let%test "(null? '(1))" =
  Eval.eval (List [ Symbol "null?"; Quote (List [ Integer 1 ]) ]) procedures = Bool false
let%test "(null? 5)" =
  Eval.eval (List [ Symbol "null?"; Integer 5 ]) procedures = Bool false

let%test "(define x 5)" =
  let e = procedures in
  let _ = Eval.eval (List [ Symbol "define"; Symbol "x"; Integer 5 ]) procedures
  in Env.get "x" e = Integer 5
let%test "(define x (+ 2 2))" =
  let e = procedures in
  let _ = Eval.eval (List [ Symbol "define"; Symbol "x"; List [ Symbol "+"; Integer 2; Integer 2 ] ]) procedures
  in Env.get "x" e = Integer 4

let%test "(equal? x 5)" =
  let e = procedures in
  let _ = Eval.eval (List [ Symbol "define"; Symbol "x"; Integer 5 ]) procedures in
  let v = Eval.eval (List [ Symbol "equal?"; Symbol "x"; Integer 5 ] ) e
  in v = Bool true
let%test "(equal? x 1)" =
  let e = procedures in
  let _ = Eval.eval (List [ Symbol "define"; Symbol "x"; Integer 5 ]) e in
  let v = Eval.eval (List [ Symbol "equal?"; Symbol "x"; Integer 1 ] ) e
  in v = Bool false

let%test "(procedure? (lambda (x) x))" =
  Eval.eval (List [ Symbol "procedure?"; List [ Symbol "lambda"; List [ Symbol "x" ]; Symbol "x" ]]) procedures = Bool true
let%test "((lambda (x) x) 42)" =
  Eval.eval (List [ List [ Symbol "lambda"; List [ Symbol "x" ]; Symbol "x" ]; Integer 42 ]) procedures = Integer 42
let%test "((lambda (x y) (+ x y)) 2 3)" =
  Eval.eval (List [ List [ Symbol "lambda"; List [ Symbol "x"; Symbol "y" ]; List [ Symbol "+"; Symbol "x"; Symbol "y" ] ]; Integer 2; Integer 3 ]) procedures = Integer 5

let%test "(if #t 1 2)" =
  Eval.eval (List [ Symbol "if"; Bool true; Integer 1; Integer 2 ]) procedures = Integer 1
let%test "(if #f 1 2)" =
  Eval.eval (List [ Symbol "if"; Bool false; Integer 1; Integer 2 ]) procedures = Integer 2
let%test "(if (> 2 1) (+ 1 3) 2)" =
  Eval.eval (List [ Symbol "if"; List [ Symbol ">"; Integer 2; Integer 1 ]; List [ Symbol "+"; Integer 1; Integer 3 ]; Integer 2 ]) procedures = Integer 4
let%test "(if (< 2 1) 2 (+ 1 3))" =
  Eval.eval (List [ Symbol "if"; List [ Symbol "<"; Integer 2; Integer 1 ]; Integer 2; List [ Symbol "+"; Integer 1; Integer 3 ] ]) procedures = Integer 4

let%test "(let () (+ 2 2))" =
  Eval.eval (List [ Symbol "let"; List []; List [ Symbol "+"; Integer 2; Integer 2] ]) procedures = Integer 4
let%test "(let ((x 1) (y 2)) (+ x y))" =
  Eval.eval (List [ Symbol "let"; List [ List [ Symbol "x"; Integer 1 ]; List [ Symbol "y"; Integer 2 ] ]; List [ Symbol "+"; Symbol "x"; Symbol "y" ] ]) procedures = Integer 3

let%test "(let* () (+ 2 2))" =
  Eval.eval (List [ Symbol "let*"; List []; List [ Symbol "+"; Integer 2; Integer 2] ]) procedures = Integer 4
let%test "(let* ((x 1) (y 2)) (+ x y))" =
  Eval.eval (List [ Symbol "let*"; List [ List [ Symbol "x"; Integer 1 ]; List [ Symbol "y"; Integer 2 ] ]; List [ Symbol "+"; Symbol "x"; Symbol "y" ] ]) procedures = Integer 3
let%test "(let* ((x 1) (y (+ x 1))) (+ x y))" =
  Eval.eval (List [ Symbol "let*"; List [ List [ Symbol "x"; Integer 1 ]; List [ Symbol "y"; List [ Symbol "+"; Symbol "x"; Integer 1 ]] ]; List [ Symbol "+"; Symbol "x"; Symbol "y" ] ]) procedures = Integer 3

let%test "(cond ((> 1 2) (+ 1 2)) (else (+ 3 4)))" =
  Eval.eval (List [ Symbol "cond"; List [ List [ Symbol ">"; Integer 1; Integer 2 ]; List [ Symbol "+"; Integer 1; Integer 2] ]; List [ Symbol "else"; List [ Symbol "+"; Integer 3; Integer 4 ] ] ]) procedures = Integer 7
