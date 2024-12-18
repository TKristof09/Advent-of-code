open Core

exception Break

let grid_size = 70 + 1

let simulate_falling t l =
    IterLabels.foldi l
      ~init:(Set.empty (module Aoc.Pair))
      ~f:(fun acc i coord ->
        if i < t then
          Set.add acc coord
        else
          acc)

let print_grid obstacles =
    for y = 0 to grid_size - 1 do
      for x = 0 to grid_size - 1 do
        if Set.mem obstacles (x, y) then
          Printf.printf "#"
        else
          Printf.printf "."
      done;
      Printf.printf "\n"
    done

let print_path obstacles prev =
    let e = (grid_size - 1, grid_size - 1) in
    let path = Hash_set.create (module Aoc.Pair) in
    let rec aux n succ =
        let p = n in
        Hash_set.add path p;
        match Hashtbl.find prev n with
        | Some x -> aux x n
        | _ -> ()
    in
    aux e e;
    for y = 0 to grid_size - 1 do
      for x = 0 to grid_size - 1 do
        if Set.mem obstacles (x, y) then
          Printf.printf "#"
        else if Hash_set.mem path (x, y) then
          Printf.printf "O"
        else
          Printf.printf "."
      done;
      Printf.printf "\n"
    done

let get_neighbours (x, y) obstacles =
    [ (0, -1); (0, 1); (-1, 0); (1, 0) ]
    |> List.filter_map ~f:(fun (dx, dy) ->
           let px, py = (x + dx, y + dy) in
           if 0 <= px && px < grid_size && 0 <= py && py < grid_size then
             if Set.mem obstacles (px, py) then
               None
             else
               Some (px, py)
           else
             None)

let djikstra s e obstacles =
    let create_tbl get_data =
        let tbl = Hashtbl.create (module Aoc.Pair) in
        for x = 0 to grid_size - 1 do
          for y = 0 to grid_size - 1 do
            if not (Set.mem obstacles (x, y)) then
              Hashtbl.set tbl ~key:(x, y) ~data:(get_data (x, y))
          done
        done;
        tbl
    in
    let dist = create_tbl (fun n -> if Aoc.Pair.equal n s then 0 else Int.max_value) in
    let prev = create_tbl (fun n -> if Aoc.Pair.equal n s then (-1, -1) else (-1, -1)) in
    let q = Hash_set.of_hashtbl_keys dist in
    let () =
        try
          while not @@ Hash_set.is_empty q do
            let cur_node =
                Hash_set.min_elt q ~compare:(fun a b ->
                    let d1 = Hashtbl.find_exn dist a in
                    let d2 = Hashtbl.find_exn dist b in
                    Int.compare d1 d2)
                |> Option.value_exn
            in
            let cur_dist = Hashtbl.find_exn dist cur_node in
            if cur_dist = Int.max_value || Aoc.Pair.equal cur_node e then raise Break;

            let x, y = cur_node in
            Hash_set.remove q cur_node;
            (* Printf.printf "%d,%d:  %d\n" x y cur_dist; *)
            get_neighbours (x, y) obstacles
            |> List.iter ~f:(fun n ->
                   (* let nx, ny = n in *)
                   let old_dist = Hashtbl.find_exn dist n in
                   let d = cur_dist + 1 in
                   if d < old_dist then (
                     (* Printf.printf "   Updating: %d,%d: %d\n" nx ny d; *)
                     Hashtbl.set dist ~key:n ~data:d;
                     Hashtbl.set prev ~key:n ~data:cur_node))
          done
        with
        | Break -> ()
    in
    if Aoc.Pair.equal (Hashtbl.find_exn prev e) (-1, -1) then
      None
    else
      Some (Hashtbl.find_exn dist e, prev)

let () =
    let obstacles_list =
        Aoc.read_to_iter "day18"
        |> IterLabels.take_while ~f:(fun s -> not @@ String.is_empty s)
        |> IterLabels.map ~f:(fun s -> String.lsplit2_exn s ~on:',' |> Tuple2.map ~f:Int.of_string)
    in

    (let obstacles = simulate_falling 1024 obstacles_list in
     match djikstra (0, 0) (grid_size - 1, grid_size - 1) obstacles with
     | None -> failwith "No paths fould"
     | Some (d, _) -> Printf.printf "Part 1: %d\n" d);
    for i = 2750 to Iter.length obstacles_list - 1 do
      let obstacles = simulate_falling i obstacles_list in
      match djikstra (0, 0) (grid_size - 1, grid_size - 1) obstacles with
      | None -> failwith @@ Printf.sprintf "Part 2: %d" i
      | Some _ -> (* print_path obstacles prev *) ()
    done
