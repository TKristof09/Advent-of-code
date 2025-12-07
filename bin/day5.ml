open Core

let is_inside n interv =
    let start, stop = interv in
    start <= n && n <= stop

let parse_intervals =
    List.map ~f:(fun s -> s |> String.lsplit2_exn ~on:'-' |> Tuple2.map ~f:Int.of_string)

(** Must be ordered, ie start1 <= start2 and unique *)
let union (start1, stop1) (start2, stop2) =
    if stop1 < start2 then
      [ (start1, stop1); (start2, stop2) ]
    else if start2 <= stop1 then
      [ (start1, max stop1 stop2) ]
    else
      assert false

let reduce_intervals l =
    let rec aux intervals =
        match intervals with
        | []
        | [ _ ] ->
            intervals
        | i1 :: i2 :: t -> (
            match union i1 i2 with
            | [ i ] -> aux (i :: t)
            | [ i1; i2 ] -> i1 :: aux (i2 :: t)
            | _ -> assert false)
    in
    aux l

let interval_size (start, stop) = stop - start + 1
let inp = Aoc.read_to_list "day5"

let part1 () =
    let intervals =
        inp |> List.take_while ~f:(fun s -> not (String.is_empty s)) |> parse_intervals
    in
    List.drop inp (List.length intervals + 1)
    |> List.map ~f:Int.of_string
    |> List.count ~f:(fun id -> List.exists intervals ~f:(is_inside id))

let part2 () =
    inp
    |> List.take_while ~f:(fun s -> not (String.is_empty s))
    |> parse_intervals
    |> List.dedup_and_sort ~compare:(fun (start1, stop1) (start2, stop2) ->
        if start1 = start2 && stop1 = stop2 then
          0
        else if start1 = start2 then
          Int.compare stop1 stop2
        else if start1 < start2 then
          -1
        else
          1)
    |> reduce_intervals
    |> List.sum (module Int) ~f:interval_size

let () =
    part1 () |> Printf.printf "Part 1: %d fresh IDs\n";
    part2 () |> Printf.printf "Part 2: %d fresh IDs\n"

let () =
    Printf.printf "Part 1: ";
    Aoc.time_fn part1;
    Printf.printf "Part 2: ";
    Aoc.time_fn part2
