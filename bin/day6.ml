open Core

type race = {
      times: int list
    ; distances: int list
  }
[@@deriving show]

let parse_info lines =
    let _, times = String.lsplit2_exn lines.(0) ~on:':' in
    let times = String.split times ~on:' ' |> List.filter_map ~f:Int.of_string_opt in
    let _, distances = String.lsplit2_exn lines.(1) ~on:':' in
    let distances = String.split distances ~on:' ' |> List.filter_map ~f:Int.of_string_opt in
    { times; distances }

(* the final distance is the function f(x) = (t - x)x  so we find (t-x)x > d *)
let find_range t d =
    let t = Int.to_float t in
    let d = Int.to_float d in
    let delta = sqrt ((t *. t) -. (4.0 *. d)) in
    let x1 = (t -. delta) /. 2.0 in
    let x2 = (t +. delta) /. 2.0 in
    (* the + 0.00...1 is because we have to win so the inequality has to be strict *)
    ( Int.of_float @@ Float.round_up (x1 +. 0.0000001)
    , Int.of_float @@ Float.round_down (x2 -. 0.0000001) )

let () =
    let race = Aoc.read_to_array_filtered "day6" |> parse_info in
    let res =
        List.fold2 race.times race.distances ~init:1 ~f:(fun acc t d ->
            let x1, x2 = find_range t d in
            Printf.printf "%d - %d\n" x1 x2 ;
            acc * (x2 - x1 + 1) )
    in
    match res with
    | Ok sum -> Printf.printf "%d\n" sum
    | Unequal_lengths -> failwith "shouldnt happen"
