open Core

type rot =
    | Left of int
    | Right of int
[@@deriving show { with_path = false }]

let parse l =
    List.map l ~f:(fun s ->
        let num = Int.of_string (String.drop_prefix s 1) in
        match s.[0] with
        | 'R' -> Right num
        | 'L' -> Left num
        | _ -> assert false)

let solve (l : rot list) =
    List.fold l ~init:(0, 50) ~f:(fun (res, cur) r ->
        (* Printf.printf "Cur: %d, op: %s\n" cur (show_rot r); *)
        match r with
        | Left n ->
            let new_val = (cur - n + Int.round_up n ~to_multiple_of:100) mod 100 in
            if new_val = 0 then (res + 1, new_val) else (res, new_val)
        | Right n ->
            let new_val = (cur + n) mod 100 in
            if new_val = 0 then (res + 1, new_val) else (res, new_val))
    |> fst

let solve2 (l : rot list) =
    List.fold l ~init:(0, 50) ~f:(fun (res, cur) r ->
        (* Printf.printf "Cur: %d, op: %s\n" cur (show_rot r); *)
        match r with
        | Left n ->
            let new_val = (cur - n + Int.round_up n ~to_multiple_of:100) mod 100 in
            if cur - n <= 0 then
              let num_passes = if cur = 0 then n / 100 else ((n - cur) / 100) + 1 in
              (* Printf.printf "passed: %d\n" num_passes; *)
              (res + num_passes, new_val)
            else
              (res, new_val)
        | Right n ->
            let new_val = (cur + n) mod 100 in
            if cur + n >= 100 then
              let num_passes = (cur + n) / 100 in
              (* Printf.printf "passed: %d\n" num_passes; *)
              (res + num_passes, new_val)
            else
              (res, new_val))
    |> fst

let () = Aoc.read_to_list "day1" |> parse |> solve |> Printf.printf "Password: %d\n"
let () = Aoc.read_to_list "day1" |> parse |> solve2 |> Printf.printf "Password 2: %d\n"
