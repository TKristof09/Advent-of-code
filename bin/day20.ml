open Core

exception Break

let neighbours m (x, y) =
    [ (-1, 0); (1, 0); (0, -1); (0, 1) ]
    |> List.filter_map ~f:(fun (dx, dy) ->
           let p = (x + dx, y + dy) in
           if Map.mem m (x + dx, y + dy) then
             Some p
           else
             None)

let get_cheat_neighbours m range (x, y) =
    let l = ref [] in
    for dx = -range to range do
      for dy = -range to range do
        if (dx <> 0 || dy <> 0) && abs dx + abs dy <= range then
          let p = (x + dx, y + dy) in
          if Map.mem m p then
            l := (x + dx, y + dy) :: !l
      done
    done;
    !l

let djikstra m ~start ~end_ =
    let create_tbl get_data =
        let tbl =
            Hashtbl.create_mapped (module Aoc.Pair) ~get_key:(fun x -> x) ~get_data (Map.keys m)
        in
        match tbl with
        | `Ok tbl -> tbl
        | `Duplicate_keys _ -> failwith "Error"
    in
    let dist = create_tbl (fun n -> if Aoc.Pair.equal n start then 0 else Int.max_value) in
    let prev = create_tbl (fun n -> if Aoc.Pair.equal n start then (-1, -1) else (-1, -1)) in
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
            if cur_dist = Int.max_value then raise Break;

            let x, y = cur_node in
            Hash_set.remove q cur_node;
            neighbours m (x, y)
            |> List.iter ~f:(fun n ->
                   let old_dist = Hashtbl.find_exn dist n in
                   let d = cur_dist + 1 in
                   if d < old_dist then (
                     Hashtbl.set dist ~key:n ~data:d;
                     Hashtbl.set prev ~key:n ~data:cur_node))
          done
        with
        | Break -> ()
    in
    (dist, prev)

let get_path prev ~from ~to_ =
    (* go in reverse because we'll have the prevs from the pov of the end node *)
    let rec aux n =
        match Hashtbl.find prev n with
        | Some x when Aoc.Pair.equal x to_ -> []
        | Some x -> x :: aux x
        | None -> failwith "No path"
    in
    from :: aux from

let get_cheat_time_save m dist node range =
    let get_dist (x, y) =
        let nx, ny = node in
        abs (nx - x) + abs (ny - y)
    in
    let orig_time = Hashtbl.find_exn dist node in
    let get_time_save p =
        match Hashtbl.find dist p with
        | None -> None
        | Some d ->
            let dt = orig_time - d - get_dist p in
            if dt <= 0 then
              None
            else
              Some dt
    in
    let l =
        get_cheat_neighbours m range node
        |> List.filter_map ~f:(fun e ->
               match get_time_save e with
               | None -> None
               | Some dt -> Some (e, dt))
        |> List.dedup_and_sort ~compare:(fun (e, _) (e', _) -> Aoc.Pair.compare e e')
    in
    (* List.iter l ~f:(fun (e, dt) -> *)
    (*     Printf.printf "%s -> %s saves %d\n" (Aoc.Pair.show node) (Aoc.Pair.show e) dt); *)
    List.count l ~f:(fun (_, dt) -> dt >= 100)

let () =
    let m =
        Aoc.read_to_map "day20" (fun c ->
            let open Char in
            c = 'S' || c = 'E' || c = '.')
    in
    let s, e =
        Map.fold m
          ~init:((0, 0), (0, 0))
          ~f:(fun ~key ~data (s, e) ->
            let open Char in
            if data = 'S' then
              (key, e)
            else if data = 'E' then
              (s, key)
            else
              (s, e))
    in
    let dist, prev = djikstra m ~start:e ~end_:s in
    let path = get_path prev ~from:s ~to_:e in
    List.fold path ~init:0 ~f:(fun acc node -> acc + get_cheat_time_save m dist node 2)
    |> Printf.printf "Part 1: %d\n";

    List.fold path ~init:0 ~f:(fun acc node -> acc + get_cheat_time_save m dist node 20)
    |> Printf.printf "Part 2: %d\n"
