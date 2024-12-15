open Core

type direction =
    | Left
    | Right
    | Up
    | Down

let parse_input l =
    match l with
    | [ map; moves ] ->
        let map =
            Aoc.string_to_map map (fun c ->
                let open Char in
                c = '#' || c = 'O' || c = '@')
        in
        let moves =
            String.concat moves
            |> String.to_list
            |> List.map ~f:(function
                 | '<' -> Left
                 | '>' -> Right
                 | '^' -> Up
                 | 'v' -> Down
                 | _ -> failwith "Invalid character")
        in
        (map, moves)
    | _ -> failwith "Invalid input"

let find_start_pos map =
    Map.fold map ~init:(-1, -1) ~f:(fun ~key:(x, y) ~data acc ->
        if Char.equal data '@' then (x, y) else acc)

let get_new_pos (x, y) dir =
    match dir with
    | Left -> (x - 1, y)
    | Right -> (x + 1, y)
    | Up -> (x, y - 1)
    | Down -> (x, y + 1)

let move1 (map, start_pos) dir =
    let rec move_aux map pos dir =
        let new_pos = get_new_pos pos dir in
        let c = Map.find_exn map pos in
        match Map.find map new_pos with
        | Some 'O' -> (
            match move_aux map new_pos dir with
            | None -> None
            | Some (map, _) ->
                let map = Map.set map ~key:new_pos ~data:c in
                Some (Map.remove map pos, new_pos))
        | Some '#' -> None
        | None
        | Some _ ->
            let map = Map.set map ~key:new_pos ~data:c in
            Some (Map.remove map pos, new_pos)
    in
    match move_aux map start_pos dir with
    | None -> (map, start_pos)
    | Some (map, pos) -> (map, pos)

let move2 (map, start_pos) dir =
    let rec move_aux map pos dir =
        let new_pos = get_new_pos pos dir in
        let c = Map.find_exn map pos in
        let push_box c' =
            match dir with
            | Left
            | Right -> (
                match move_aux map new_pos dir with
                | None -> None
                | Some (map, _) ->
                    let map = Map.set map ~key:new_pos ~data:c in
                    Some (Map.remove map pos, new_pos))
            | Up
            | Down -> (
                let other_box =
                    match c' with
                    | '[' -> (fst new_pos + 1, snd new_pos)
                    | ']' -> (fst new_pos - 1, snd new_pos)
                    | _ -> failwith "Impossible"
                in
                let first_move =
                    match move_aux map new_pos dir with
                    | None -> None
                    | Some (map, _) ->
                        let map = Map.set map ~key:new_pos ~data:c in
                        Some (Map.remove map pos, new_pos)
                in
                match first_move with
                | None -> None
                | Some (map', _) -> (
                    match move_aux map' other_box dir with
                    | None -> None
                    | Some (map, _) ->
                        let map = Map.set map ~key:new_pos ~data:c in
                        Some (Map.remove map pos, new_pos)))
        in
        match Map.find map new_pos with
        | Some '[' -> push_box '['
        | Some ']' -> push_box ']'
        | Some '#' -> None
        | None
        | Some _ ->
            let map = Map.set map ~key:new_pos ~data:c in
            Some (Map.remove map pos, new_pos)
    in
    match move_aux map start_pos dir with
    | None -> (map, start_pos)
    | Some (map, pos) -> (map, pos)

let print_grid map =
    let (sx, sy), _ = Map.max_elt_exn map in
    for y = 0 to sy do
      for x = 0 to sx do
        match Map.find map (x, y) with
        | None -> Printf.printf "."
        | Some c -> Printf.printf "%c" c
      done;
      Printf.printf "\n"
    done;
    map

let calc_score map =
    Map.fold map ~init:0 ~f:(fun ~key:(x, y) ~data acc ->
        if Char.(data = 'O' || data = '[') then
          acc + x + (y * 100)
        else
          acc)

let widen_map map =
    Map.fold map
      ~init:(Map.empty (module Aoc.Pair))
      ~f:(fun ~key:(x, y) ~data acc ->
        let nx, ny = (x * 2, y) in
        match data with
        | '#' -> Map.set acc ~key:(nx, ny) ~data:'#' |> Map.set ~key:(nx + 1, ny) ~data:'#'
        | '@' -> Map.set acc ~key:(nx, ny) ~data:'@'
        | 'O' -> Map.set acc ~key:(nx, ny) ~data:'[' |> Map.set ~key:(nx + 1, ny) ~data:']'
        | _ -> failwith "Invalid character")

let () =
    let map, moves =
        Aoc.read_to_iter "day15"
        |> Aoc.split_iter ~on:String.is_empty
        |> Iter.to_list
        |> parse_input
    in
    let start_pos = find_start_pos map in
    Printf.printf "-------------PART 1-------------\n";
    List.fold moves ~init:(map, start_pos) ~f:move1
    |> Tuple2.get1
    |> print_grid
    |> calc_score
    |> Printf.printf "%d\n";
    Printf.printf "-------------PART 2-------------\n";
    let map = widen_map map in
    let start_pos = find_start_pos map in
    List.fold moves ~init:(map, start_pos) ~f:move2
    |> Tuple2.get1
    |> print_grid
    |> calc_score
    |> Printf.printf "%d\n"
