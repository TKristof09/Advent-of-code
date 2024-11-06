open Core

type map = {
      network: (string * string) String.Map.t
    ; instructions: char list
    ; starts: string list
  }

let show_network network =
    [%derive.show: (string * (string * string)) list] (Map.to_alist network ~key_order:`Decreasing)

let show_map map =
    Printf.sprintf "Instructions: %s\nStarts:%s\n%s"
      ([%derive.show: char list] map.instructions)
      ([%derive.show: string list] map.starts)
      (show_network map.network)

let parse_network lines =
    let instructions = List.hd_exn lines |> String.to_list in
    let network, starts =
        List.fold (List.tl_exn lines) ~init:(String.Map.empty, []) ~f:(fun (m, starts) line ->
            let name, children = String.lsplit2_exn line ~on:'=' in
            let name = String.filter name ~f:Char.is_alphanum in
            let l, r =
                String.lsplit2_exn children ~on:','
                |> Tuple2.map ~f:(String.filter ~f:Char.is_alphanum)
            in
            if Char.compare name.[2] 'A' = 0 then
              (Map.add_exn m ~key:name ~data:(l, r), name :: starts)
            else
              (Map.add_exn m ~key:name ~data:(l, r), starts) )
    in
    { network; instructions; starts }

let traverse_map_1 map =
    let rec aux cur_loc instr =
        match (cur_loc, instr) with
        | "ZZZ", _ -> 0
        | _, [] -> aux cur_loc map.instructions
        | _, d :: t ->
            Printf.printf "%s -%c-> " cur_loc d ;
            let l, r = Map.find_exn map.network cur_loc in
            let open Core.Char in
            if d = 'L' then
              1 + aux l t
            else
              1 + aux r t
    in
    aux "AAA" map.instructions

let traverse_map_2 map =
    let rec aux cur_locs instr i =
        (* Printf.printf "%s\n" ([%derive.show: string list]cur_locs); *)
        if List.for_all cur_locs ~f:(fun s -> Char.compare s.[2] 'Z' = 0) then (
          Printf.printf "found\n" ; i )
        else
          match instr with
          | [] -> aux cur_locs map.instructions i
          | d :: t ->
              (* Printf.printf "%c " d; *)
              let new_locs =
                  List.map cur_locs ~f:(fun loc ->
                      match Map.find map.network loc with
                      | None -> failwith "not found"
                      | Some (l, r) ->
                          let open Core.Char in
                          if d = 'L' then
                            l
                          else
                            r )
              in
              aux new_locs t (i + 1)
    in
    aux map.starts map.instructions 0

let get_dist_end map start_loc =
    let rec walk_path map start_loc cur_loc instr i =
        if Char.compare cur_loc.[2] 'Z' = 0 && i <> 0 then
          (cur_loc, i)
        else
          match (cur_loc, instr) with
          | _, [] -> walk_path map start_loc cur_loc map.instructions i
          | _, d :: t ->
              (* Printf.printf "%s -%c-> " cur_loc d; *)
              let l, r = Map.find_exn map.network cur_loc in
              let open Core.Char in
              if d = 'L' then
                walk_path map start_loc l t (i + 1)
              else
                walk_path map start_loc r t (i + 1)
    in
    walk_path map start_loc start_loc map.instructions 0

let get_period map start_loc =
    let rec walk_path map start_loc cur_loc instr i =
        match (cur_loc, instr) with
        | loc, _ when String.compare loc start_loc = 0 && i <> 0 -> i
        | _, [] -> walk_path map start_loc cur_loc map.instructions i
        | _, d :: t ->
            (* Printf.printf "%s -%c-> " cur_loc d; *)
            let l, r = Map.find_exn map.network cur_loc in
            let open Core.Char in
            if d = 'L' then
              walk_path map start_loc l t (i + 1)
            else
              walk_path map start_loc r t (i + 1)
    in
    walk_path map start_loc start_loc map.instructions 0

let rec gcd u v =
    if v <> 0 then
      gcd v (u mod v)
    else
      abs u

let lcm m n =
    match (m, n) with
    | 0, _
     |_, 0 ->
        0
    | m, n -> abs (m * n) / gcd m n

let () =
    let map = Aoc.read_to_list_filtered "day8" |> parse_network in
    (* Printf.printf "%s\n" (show_map map) ; *)
    (* List.iter map.starts ~f:(fun loc ->  *)
    (*     let end_node, len =  get_dist_end map loc in *)
    (*     Printf.printf "%s: %d ; %d\n" loc len (get_period map end_node) *)
    (* ) *)
    List.map map.starts ~f:(fun loc -> snd @@ get_dist_end map loc)
    |> List.fold ~init:1 ~f:lcm
    |> Printf.printf "%d\n"
