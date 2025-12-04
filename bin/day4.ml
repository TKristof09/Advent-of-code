open Core

let count_neighbours x y s = Aoc.eight_neighbours x y |> List.count ~f:(Set.mem s)

let clear_out s =
    let rec aux s sum =
        let new_s = Set.filter s ~f:(fun (x, y) -> count_neighbours x y s >= 4) in
        let new_count = Set.length new_s
        and old_count = Set.length s in
        if new_count = old_count then
          sum
        else
          aux new_s (old_count - new_count + sum)
    in
    aux s 0

let locations = Aoc.read_to_map "day4" (fun c -> Char.equal c '@') |> Map.key_set

let part1 () =
    locations
    |> Set.count ~f:(fun (x, y) -> count_neighbours x y locations < 4)
    |> Printf.printf "Part 1: %d rolls can be removed\n"

let part2 () = locations |> clear_out |> Printf.printf "Part 2: %d rolls can be removed\n"

let () =
    Aoc.time_fn part1;
    Aoc.time_fn part2
