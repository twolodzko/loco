
module LocalEnv = Map.Make(String)

type 'a t = 'a LocalEnv.t ref list

let of_env e = (ref LocalEnv.empty) :: e

let of_list lst = [ ref (lst |> List.to_seq |> LocalEnv.of_seq) ]

let rec get k e =
  match e with
  | e :: tail -> (
      try LocalEnv.find k !e
      with Not_found -> get k tail
    )
  | [] -> raise Not_found

let add k v e =
  match e with
  | e :: _ ->
      e := LocalEnv.add k v !e
  | [] -> raise Not_found

let update k v e =
  let has_key k e =
    match LocalEnv.find_opt k !e with
    | Some _ -> true
    | None -> false
  in let rec impl = function
    | e :: tail ->
        if (has_key k e) then
          e := LocalEnv.add k v !e
        else impl tail
    | [] -> raise Not_found
  in impl e
