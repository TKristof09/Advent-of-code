open Core

let _solve1 m =
    let neighbours = [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ] in
    let find_xmas m coord =
        List.fold neighbours ~init:0 ~f:(fun acc dir ->
            let neighbour = (fst coord + fst dir, snd coord + snd dir) in
            match Map.find m neighbour with
            | Some 'M' -> (
                let n1 = (fst neighbour + fst dir, snd neighbour + snd dir) in
                let n2 = (fst n1 + fst dir, snd n1 + snd dir) in
                match (Map.find m n1, Map.find m n2) with
                | Some 'A', Some 'S' -> acc + 1
                | _ -> acc)
            | _ -> acc)
    in
    Map.fold m ~init:0 ~f:(fun ~key ~data acc ->
        if Char.equal data 'X' then
          find_xmas m key + acc
        else
          acc)

let solve2 m =
    let neighbours = [ (-1, -1); (-1, 1); (1, -1); (1, 1) ] in
    let find_xmas m (cx, cy) =
        let find_mas (cx, cy) (dx, dy) =
            let neighbour = (cx + dx, cy + dy) in
            match Map.find m neighbour with
            | Some 'M' -> (
                let n2 = (cx - dx, cy - dy) in
                match Map.find m n2 with
                | Some 'S' -> true
                | _ -> false)
            | _ -> false
        in
        List.fold neighbours ~init:false ~f:(fun acc (dx, dy) ->
            if find_mas (cx, cy) (dx, dy) then
              acc || find_mas (cx, cy) (dx, -dy) || find_mas (cx, cy) (-dx, dy)
            else
              acc)
    in
    Map.fold m ~init:0 ~f:(fun ~key ~data acc ->
        if Char.equal data 'A' then
          if find_xmas m key then
            1 + acc
          else
            acc
        else
          acc)

let () =
    let input =
        Aoc.read_to_map "day4" (fun c ->
            let open Core.Char in
            c = 'M' || c = 'A' || c = 'S')
    in
    (* Printf.printf "%s" (Aoc.show_coord_map input); *)
    solve2 input |> Printf.printf "%d\n"
