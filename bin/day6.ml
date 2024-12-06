open Core

let _print_grid grid (size_x, size_y) =
    for y = 0 to size_y - 1 do
      for x = 0 to size_x - 1 do
        match Map.find grid (x, y) with
        | None -> Printf.printf "."
        | Some c -> Printf.printf "%c" c
      done;
      Printf.printf "\n"
    done;
    Printf.printf "\n"

let rec move grid (x, y) (dx, dy) =
    let turn90 dx dy =
        match (dx, dy) with
        | 0, -1 -> (1, 0) (* Up *)
        | 1, 0 -> (0, 1) (* Right *)
        | 0, 1 -> (-1, 0) (* Down *)
        | -1, 0 -> (0, -1) (* Left *)
        | _ -> failwith "Invalid direction"
    in
    let new_pos = (x + dx, y + dy) in
    match Map.find grid new_pos with
    | None -> (new_pos, (dx, dy))
    | Some _ ->
        let dx', dy' = turn90 dx dy in
        move grid (x, y) (dx', dy')

let in_grid pos (size_x, size_y) =
    let x, y = pos in
    0 <= x && x < size_x && 0 <= y && y < size_y

let gogogo grid s grid_size =
    let cur = ref s in
    let cur_dir = ref (0, -1) in
    let s = ref (Set.empty (module Aoc.Pair)) in
    (* let grid_visu = ref grid in *)
    while in_grid !cur grid_size do
      s := Set.add !s !cur;
      (* grid_visu := Map.set !grid_visu ~key:!cur ~data:'X'; *)
      let new_pos, new_dir = move grid !cur !cur_dir in
      cur := new_pos;
      cur_dir := new_dir
    done;
    (* print_grid !grid_visu grid_size; *)
    !s

let is_cycle grid start grid_size =
    let is_cycle tbl pos (dx, dy) =
        match Hashtbl.find tbl pos with
        | None -> false
        | Some l ->
            List.filter l ~f:(fun (dx', dy') -> dx = dx' && dy = dy') |> List.is_empty |> not
    in
    let cur = ref start in
    let cur_dir = ref (0, -1) in
    let already_visited = Hashtbl.create (module Aoc.Pair) in
    (* let grid_visu = ref grid in *)
    while in_grid !cur grid_size && not (is_cycle already_visited !cur !cur_dir) do
      (* grid_visu := Map.set !grid_visu ~key:!cur ~data:'X'; *)
      let x, y = !cur in
      let dx, dy = !cur_dir in
      Hashtbl.update already_visited (x, y) ~f:(fun v ->
          match v with
          | None -> [ (dx, dy) ]
          | Some l -> (dx, dy) :: l);
      let new_pos, new_dir = move grid !cur !cur_dir in
      cur := new_pos;
      cur_dir := new_dir
    done;
    (* print_grid !grid_visu grid_size *)
    is_cycle already_visited !cur !cur_dir

let find_possible_obstacles grid s path grid_size =
    Set.fold (Set.remove path s) ~init:0 ~f:(fun acc (x, y) ->
        let grid' = Map.set grid ~key:(x, y) ~data:'O' in
        if is_cycle grid' s grid_size then
          acc + 1
        else
          acc)

let () =
    let grid = Aoc.read_to_map "day6" (fun c -> Char.equal c '#' || Char.equal c '^') in
    let input = Aoc.read_to_array "day6" in
    let grid_size = (String.length input.(0), Array.length input) in
    let starting_pos =
        Map.fold_until grid ~init:(-1, -1)
          ~f:(fun ~key ~data acc -> if Char.equal data '^' then Stop key else Continue acc)
          ~finish:(fun _ -> failwith "Couldn't find starting pos")
    in
    let grid = Map.remove grid starting_pos in
    (* print_grid grid grid_size; *)
    (* Printf.printf "\n"; *)
    let path = gogogo grid starting_pos grid_size in
    Printf.printf "Part 1: %d\n" (Set.length path);
    find_possible_obstacles grid starting_pos path grid_size |> Printf.printf "Part 2: %d\n"
