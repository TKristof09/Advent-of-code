open Core

let parse_card_wins line =
    let card, numbers = String.lsplit2_exn line ~on:':' in
    let id = String.split card ~on:' ' |> List.last_exn |> Int.of_string in
    let winning, picked = String.lsplit2_exn numbers ~on:'|' in
    let winning =
        List.filter_map (String.split winning ~on:' ') ~f:(fun x ->
            match x with
            | "" -> None
            | s -> Some (Int.of_string s) )
    and picked =
        List.filter_map (String.split picked ~on:' ') ~f:(fun x ->
            match x with
            | "" -> None
            | s -> Some (Int.of_string s) )
    in
    let num_common = Set.inter (Int.Set.of_list winning) (Int.Set.of_list picked) |> Set.length in
    if num_common > 0 then (
      Printf.printf "%d: %d\n" id num_common ;
      num_common )
    else
      0

(* let () = *)
(*     let lines = Aoc.read_to_list "day4" in *)
(*     List.fold lines ~init:0 ~f:(fun acc line -> acc +  *)
(*         match parse_card_wins line with *)
(*             | 0 -> 0 *)
(*             | n -> Int.pow 2 (n - 1) *)
(*         ) *)
(*     |> Printf.printf "%d\n"  *)

let part_two wins =
    let cards_owned = List.init (Array.length wins) ~f:(fun _ -> 1) in
    let rec update_next num_won n cards =
        match cards with
        | [] -> []
        | x :: t ->
            if n > 0 then
              (x + num_won) :: update_next num_won (n - 1) t
            else
              cards
    in
    let rec aux l i =
        match l with
        | [] -> []
        | x :: t -> x :: (aux (update_next x wins.(i) t)) (i + 1)
    in
    aux cards_owned 0 |> List.fold ~init:0 ~f:( + )

let () =
    let lines = Aoc.read_to_array_filtered "day4" in
    let wins = Array.map lines ~f:parse_card_wins in
    part_two wins |> Printf.printf "%d\n"
