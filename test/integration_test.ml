
let%test _ = Eval.eval_stream (Reader.of_string "(define x 1) (+ x 2)") Scheme.procedures = Integer 3

let%test _ =
  let code = "
  (define x 5)
  (define foo (lambda (y) (+ x y)))
  (foo 4)
  " in Eval.eval_stream (Reader.of_string code) Scheme.procedures = Integer 9
let%test _ =
  let code = "
  (define x 5)
  (define foo (lambda (y) (+ x y)))
  ((lambda (x) (foo x)) 6)
  " in Eval.eval_stream (Reader.of_string code) Scheme.procedures = Integer 11
let%test _ =
  let code = "
    (define x 5)
    (define foo (lambda (y) (+ x y)))
    (define x 1)
    (foo 4)
  " in Eval.eval_stream (Reader.of_string code) Scheme.procedures = Integer 5
let%test _ =
  let code = "
    (define fibo (lambda (n)
      (if (= n 0) 0
        (if (= n 1) 1
          (+ (fibo (- n 1))
            (fibo (- n 2)))))))
    (fibo 9)
  " in Eval.eval_stream (Reader.of_string code) Scheme.procedures = Integer 34
let%test _ =
  let code = "
    (define impl (lambda (it second first)
        (if (= it 0) first
          (impl (- it 1) (+ first second) second))))
    (define fibo (lambda (n) (impl n 1 0)))
    (fibo 9)
  " in Eval.eval_stream (Reader.of_string code) Scheme.procedures = Integer 34
let%test _ =
  let code = "
  ;; start
  (define x  ; (/ 1 0)
    5)
  ;; (/ 1 0)
  (define foo ;; (/ 1 0)
    (lambda (y) (+ x y))
               ;; (/ 1 0)
          )
  (foo 4)
  ;; end
  " in Eval.eval_stream (Reader.of_string code) Scheme.procedures = Integer 9
let%test _ =
  let code = "
  (define x 5)
  (let ()
    (set! x 10))
  x
  " in Eval.eval_stream (Reader.of_string code) Scheme.procedures = Integer 10
