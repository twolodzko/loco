
let print_error err =
  let msg = Printexc.to_string err
  and stack = Printexc.get_backtrace () in
    Printf.printf "Error: %s\n%s" msg stack

let try_eval_print stm env =
  try
    let inp = Parser.read_sexpr stm in
    let out = Eval.eval inp env in
    Types.print_sexpr out;
    print_newline ()
  with
  | Stream.Failure -> exit 0
  | err -> print_error err

let rec repl stm env =
  print_string "> ";
  flush stdout;
  try_eval_print stm env;
  repl stm env

let () =
  let env = Env.of_env Scheme.procedures in
  if (Array.length Sys.argv) = 1 then (
    print_string "Press ^D to exit\n";
    repl (Reader.of_channel stdin) env
  ) else (
    for i = 1 to Array.length Sys.argv - 1 do
      Types.print_sexpr (Eval.eval_file Sys.argv.(i) env);
      print_newline ()
    done
  )
