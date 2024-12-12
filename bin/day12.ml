open Core

let rec calc_region1 m (x, y) c visited perim area =
    let neighbours =
        [ (-1, 0); (1, 0); (0, -1); (0, 1) ]
        |> List.map ~f:(fun (dx, dy) -> (x + dx, y + dy))
        |> List.filter ~f:(fun p ->
               match Map.find m p with
               | None -> false
               | Some c' -> Char.equal c c')
    in
    let num_perim = 4 - List.length neighbours in
    List.fold neighbours
      ~init:(Set.add visited (x, y), perim + num_perim, area + 1)
      ~f:(fun (visited, perim, area) p ->
        if Set.mem visited p then
          (visited, perim, area)
        else
          calc_region1 m p c visited perim area)

let calculate_corners m (x, y) c =
    let rotate90 (x, y) = (-y, x) in
    let n1 = [ (-1, 0); (0, -1); (-1, -1) ] in
    let n2 = List.map n1 ~f:rotate90 in
    let n3 = List.map n2 ~f:rotate90 in
    let n4 = List.map n3 ~f:rotate90 in
    let check l =
        let neighbours =
            List.map l ~f:(fun (dx, dy) ->
                let p = (x + dx, y + dy) in
                match Map.find m p with
                | None -> false
                | Some c' -> Char.equal c c')
        in
        match neighbours with
        (* convex corner *)
        (* ?Y *)
        (* YX *)
        | [ false; false; _ ] -> 1
        (* concave corner *)
        (* YX *)
        (* XX *)
        | [ true; true; false ] -> 1
        | _ -> 0
    in
    check n1 + check n2 + check n3 + check n4

let rec calc_region2 m (x, y) c visited sides area =
    let neighbours =
        [ (-1, 0); (1, 0); (0, -1); (0, 1) ]
        |> List.filter ~f:(fun (dx, dy) ->
               let p = (x + dx, y + dy) in
               match Map.find m p with
               | None -> false
               | Some c' -> Char.equal c c')
    in
    let num_sides = calculate_corners m (x, y) c in
    (* Printf.printf "   (%d,%d) -> %d\n" x y num_sides; *)
    List.fold neighbours
      ~init:(Set.add visited (x, y), sides + num_sides, area + 1)
      ~f:(fun (visited, sides, area) (dx, dy) ->
        let p = (x + dx, y + dy) in
        if Set.mem visited p then
          (visited, sides, area)
        else
          calc_region2 m p c visited sides area)

let solve1 m =
    Map.fold m
      ~init:(Set.empty (module Aoc.Pair), 0)
      ~f:(fun ~key ~data (visited, res) ->
        if Set.mem visited key then
          (visited, res)
        else
          let visited', cur_perim, cur_area = calc_region1 m key data visited 0 0 in
          (visited', res + (cur_perim * cur_area)))
    |> Tuple2.get2

let solve2 m =
    Map.fold m
      ~init:(Set.empty (module Aoc.Pair), 0)
      ~f:(fun ~key ~data (visited, res) ->
        if Set.mem visited key then
          (visited, res)
        else
          let visited', cur_sides, cur_area = calc_region2 m key data visited 0 0 in
          (* Printf.printf "%c -- Sides: %d, Area: %d\n" data cur_sides cur_area; *)
          (visited', res + (cur_sides * cur_area)))
    |> Tuple2.get2

let () =
    let m = Aoc.read_to_map "day12" (fun _ -> true) in
    m |> solve1 |> Printf.printf "Part 1: %d\n";
    m |> solve2 |> Printf.printf "Part 2: %d\n"
