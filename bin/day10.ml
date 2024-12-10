open Core

let parse_input () =
    Aoc.read_to_array "day10"
    |> Array.foldi
         ~init:(Map.empty (module Aoc.Pair), [])
         ~f:(fun y (m, l) line ->
           String.foldi line ~init:(m, l) ~f:(fun x (m', l) c ->
               if Char.equal c '0' then
                 (m', (x, y) :: l)
               else
                 (Map.set m' ~key:(x, y) ~data:(Char.to_int c - Char.to_int '0'), l)))

let rec bfs_part1 map (x, y) height =
    let neighbours =
        [ (-1, 0); (1, 0); (0, -1); (0, 1) ] |> List.map ~f:(fun (dx, dy) -> (x + dx, y + dy))
    in
    if height = 9 then
      Set.singleton (module Aoc.Pair) (x, y)
    else
      List.fold neighbours
        ~init:(Set.empty (module Aoc.Pair))
        ~f:(fun acc p ->
          match Map.find map p with
          | None -> acc
          | Some h when h = height + 1 -> Set.union acc (bfs_part1 map p h)
          | Some h -> acc)

let rec bfs_part2 map (x, y) height =
    let neighbours =
        [ (-1, 0); (1, 0); (0, -1); (0, 1) ] |> List.map ~f:(fun (dx, dy) -> (x + dx, y + dy))
    in
    if height = 9 then
      1
    else
      List.fold neighbours ~init:0 ~f:(fun acc p ->
          match Map.find map p with
          | None -> acc
          | Some h when h = height + 1 -> acc + bfs_part2 map p h
          | Some h -> acc)

let () =
    let map, trail_heads = parse_input () in
    (* List.fold trail_heads ~init:0 ~f:(fun acc (x, y) -> acc + (bfs_part1 map (x, y) 0 |> Set.length)) *)
    List.fold trail_heads ~init:0 ~f:(fun acc (x, y) -> acc + bfs_part2 map (x, y) 0)
    |> Printf.printf "%d\n"
