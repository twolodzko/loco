open Env

let%test _ = (
    let e1 = LocalEnv.empty in
    let e1 = LocalEnv.add "x" 1 e1 in
    let e2 = LocalEnv.empty in
    let e2 = LocalEnv.add "y" 2 e2 in
    let envs = [ref e1; ref e2] in
    get "x" envs
) = 1

let%test _ = (
    let e1 = LocalEnv.empty in
    let e1 = LocalEnv.add "x" 1 e1 in
    let e2 = LocalEnv.empty in
    let e2 = LocalEnv.add "y" 2 e2 in
    let envs = [ref e1; ref e2] in
    get "y" envs
) = 2

let%test _ = (
    let e1 = LocalEnv.empty in
    let e1 = LocalEnv.add "x" 1 e1 in
    let e2 = LocalEnv.empty in
    let e2 = LocalEnv.add "y" 2 e2 in
    let envs = [ref e1; ref e2] in
    get "y" envs
) = 2

let%test _ = (
    let e1 = LocalEnv.empty in
    let e1 = LocalEnv.add "x" 1 e1 in
    let e2 = LocalEnv.empty in
    let e2 = LocalEnv.add "x" 2 e2 in
    let envs = [ref e1; ref e2] in
    get "x" envs
) = 1

let%test _ = (
    let envs = [ref LocalEnv.empty] in
    add "x" 1 envs;
    get "x" envs
) = 1

let%test _ = (
    let e1 = LocalEnv.empty in
    let e1 = LocalEnv.add "x" 1 e1 in
    let e2 = LocalEnv.empty in
    let e2 = LocalEnv.add "y" 2 e2 in
    let envs = [ref e1; ref e2] in
    add "z" 3 envs;
    get "z" envs
) = 3

let%test _ = (
    let e1 = LocalEnv.empty in
    let e1 = LocalEnv.add "x" 1 e1 in
    let e2 = LocalEnv.empty in
    let e2 = LocalEnv.add "y" 2 e2 in
    let envs = [ref e1; ref e2] in
    update "y" 3 envs;
    LocalEnv.find_opt "y" !(List.hd envs)
) = None

let%test _ = (
    let e1 = LocalEnv.empty in
    let e1 = LocalEnv.add "x" 1 e1 in
    let e2 = LocalEnv.empty in
    let e2 = LocalEnv.add "y" 2 e2 in
    let envs = [ref e1; ref e2] in
    update "y" 3 envs;
    LocalEnv.find_opt "x" !(List.hd envs)
) = Some 1

let%test _ = (
    let e1 = LocalEnv.empty in
    let e1 = LocalEnv.add "x" 1 e1 in
    let e2 = LocalEnv.empty in
    let e2 = LocalEnv.add "y" 2 e2 in
    let envs = [ref e1; ref e2] in
    update "y" 3 envs;
    LocalEnv.find_opt "y" !(List.nth envs 1)
) = Some 3

let%test _ = (
    let e1 = LocalEnv.empty in
    let e1 = LocalEnv.add "x" 1 e1 in
    let e2 = LocalEnv.empty in
    let e2 = LocalEnv.add "y" 2 e2 in
    let envs = [ref e1; ref e2] in
    try let _ = update "z" 3 envs in false
    with Not_found -> true
)

let%test _ = let e = of_list [("x", 1); ("y", 2); ("z", 3)] in get "x" e = 1
let%test _ = let e = of_list [("x", 1); ("y", 2); ("z", 3)] in get "y" e = 2
let%test _ = let e = of_list [("x", 1); ("y", 2); ("z", 3)] in get "z" e = 3
let%test _ =
    let e = of_list [("x", 1); ("y", 2); ("z", 3)] in
    try let _ = get "a" e in false
    with Not_found -> true
