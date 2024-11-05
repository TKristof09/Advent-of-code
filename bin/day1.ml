open Core

let part_one s =
    let digits =
        String.fold s ~init:[] ~f:(fun acc ch ->
            match Char.get_digit ch with
            | None -> acc
            | Some x -> x :: acc )
    in
    match digits with
    | [] -> 0
    | x :: [] -> (x * 10) + x
    | _ -> List.hd_exn digits + (List.last_exn digits * 10)

let print_list l = List.iter l ~f:(fun x -> Printf.printf "%d " x)

let part_two s =
    let rec aux l =
        match l with
        | [] -> []
        | ch :: t when Char.is_digit ch -> Char.get_digit_exn ch :: aux t
        | 'o' :: 'n' :: 'e' :: _ -> 1 :: aux (List.tl_exn l)
        | 't' :: 'w' :: 'o' :: _ -> 2 :: aux (List.tl_exn l)
        | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: _ -> 3 :: aux (List.tl_exn l)
        | 'f' :: 'o' :: 'u' :: 'r' :: _ -> 4 :: aux (List.tl_exn l)
        | 'f' :: 'i' :: 'v' :: 'e' :: _ -> 5 :: aux (List.tl_exn l)
        | 's' :: 'i' :: 'x' :: _ -> 6 :: aux (List.tl_exn l)
        | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: _ -> 7 :: aux (List.tl_exn l)
        | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: _ -> 8 :: aux (List.tl_exn l)
        | 'n' :: 'i' :: 'n' :: 'e' :: _ -> 9 :: aux (List.tl_exn l)
        | _ :: t -> aux t
    in
    let digits = aux (String.to_list s) in
    match digits with
    | [] -> 0
    | x :: [] -> (x * 10) + x
    | _ -> (List.hd_exn digits * 10) + List.last_exn digits

(* let () =  *)
(*     let input_lines = Aoc.read_to_list "day1" in *)
(*     let result = List.fold_left input_lines ~init:0 ~f:(fun acc line -> acc + (part_one line)) in *)
(*     Printf.printf "%d\n" result *)

let () =
    let input_lines = Aoc.read_to_list_filtered "day1" in
    let result = List.fold input_lines ~init:0 ~f:(fun acc line -> acc + part_two line) in
    Printf.printf "%d\n" result
