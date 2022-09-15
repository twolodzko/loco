open Types

exception Parsing_error

let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false

let rec skip_whitespace stm =
  match Stream.peek stm with
  | Some ' ' | Some '\t' | Some '\n' | Some '\r' ->
      Stream.junk stm;
      skip_whitespace stm
  | _ -> ()

let is_word_boundary = function
  | '(' | ')' | '\'' | '`' | ',' | '"' -> true
  | s -> is_whitespace s

let bool_of_string = function
  | "#t" -> true
  | "#f" -> false
  | _ -> raise Parsing_error

let read_word stm =
  let rec impl acc =
    match Stream.peek stm with
    | Some c ->
        if is_word_boundary c then acc
        else (
          Stream.junk stm;
          impl (acc ^ Char.escaped c)
        )
    | None -> acc
  in impl ""

let read_string stm =
  let rec impl acc =
    match Stream.next stm with
    | '\\' -> impl (acc ^ Char.escaped (Stream.next stm))
    | '"' -> acc
    | c -> impl (acc ^ Char.escaped c)
  in impl ""

let rec read_sexpr stm =
  skip_whitespace stm;
  match Stream.npeek 2 stm with
  | '0' .. '9' :: _ | ['-'; '0' .. '9'] | ['+'; '0' .. '9'] -> (
      let v = read_word stm in
      try
        Integer (int_of_string v)
      with _ -> Symbol v
  )
  | ['#'; 't'] | ['#'; 'f'] ->
      Bool (bool_of_string (read_word stm))
  | '\'' :: _ ->
      Stream.junk stm;
      Quote (read_sexpr stm)
  | '(' :: _ ->
      Stream.junk stm;
      List (read_list stm [])
  | '"' :: _ ->
      Stream.junk stm;
      String (read_string stm)
  | ')' :: _ ->
      raise Parsing_error
  | [] ->
      raise Stream.Failure
  | _ ->
      Symbol (read_word stm)

and read_list stm lst =
  skip_whitespace stm;
  match Stream.peek stm with
  | Some ')' ->
      Stream.junk stm;
      List.rev lst
  | Some _ ->
      read_list stm (read_sexpr stm :: lst)
  | None ->
      raise Stream.Failure
