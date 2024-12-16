open Core

type direction =
    | Up
    | Down
    | Left
    | Right
[@@deriving compare, sexp, hash, show, equal]

let directions = [ Up; Down; Left; Right ]

module Node = struct
  type t = direction * (int * int) [@@deriving compare, sexp, hash, show, equal]
end

let get_coord (x, y) d =
    match d with
    | Up -> (x, y - 1)
    | Down -> (x, y + 1)
    | Left -> (x - 1, y)
    | Right -> (x + 1, y)

let cost old_dir new_dir =
    match (old_dir, new_dir) with
    | Up, Up
    | Down, Down
    | Left, Left
    | Right, Right ->
        1 (* No turn *)
    | Up, Down
    | Down, Up
    | Left, Right
    | Right, Left ->
        2001
    | Up, Left
    | Up, Right
    | Down, Left
    | Down, Right
    | Left, Up
    | Left, Down
    | Right, Up
    | Right, Down ->
        1001

let get_node_with_dirs g n =
    List.zip_exn directions [ n; n; n; n ] |> List.filter_map ~f:(fun x -> Hashtbl.find g x)

let print_path map paths =
    let map = Hash_set.fold paths ~init:map ~f:(fun map p -> Map.set map ~key:p ~data:'-') in
    let (sx, sy), _ = Map.max_elt_exn map in
    for y = 0 to sy do
      for x = 0 to sx do
        match Map.find_exn map (x, y) with
        | '.' -> Printf.printf " "
        | '#' -> Printf.printf "#"
        | '-' -> Printf.printf "."
        | _ -> ()
      done;
      Printf.printf "\n"
    done

let djikstra g s (e : Aoc.Pair.t) =
    let s = (Right, s) in
    let create_tbl get_data =
        let tbl =
            Hashtbl.create_mapped (module Node) ~get_key:(fun x -> x) ~get_data (Hashtbl.keys g)
        in
        match tbl with
        | `Ok tbl -> tbl
        | `Duplicate_keys _ -> failwith "Error"
    in
    let dist = create_tbl (fun n -> if Node.equal n s then 0 else Int.max_value) in
    let prev = create_tbl (fun n -> if Node.equal n s then [] else []) in
    let q = Hash_set.of_hashtbl_keys dist in
    Hash_set.add q s;
    while not @@ Hash_set.is_empty q do
      let cur_node =
          Hash_set.min_elt q ~compare:(fun a b ->
              let d1 = Hashtbl.find_exn dist a in
              let d2 = Hashtbl.find_exn dist b in
              Int.compare d1 d2)
          |> Option.value_exn
      in
      let cur_dir, (x, y) = cur_node in
      Hash_set.remove q cur_node;
      let cur_dist = Hashtbl.find_exn dist cur_node in
      (* Printf.printf "%d,%d: %s -- %d\n" x y (show_direction cur_dir) cur_dist; *)
      Hashtbl.find_exn g cur_node
      |> List.filter ~f:(fun n -> not (Node.equal n cur_node))
      |> List.iter ~f:(fun n ->
             let dir, (nx, ny) = n in
             let old_dist = Hashtbl.find_exn dist n in
             let d = cur_dist + cost cur_dir dir in
             if d < old_dist then (
               (* Printf.printf "   Updating: %s -- %d,%d: %d\n" (show_direction dir) nx ny d; *)
               Hashtbl.set dist ~key:n ~data:d;
               Hashtbl.set prev ~key:n ~data:[ cur_node ])
             else if d = old_dist then
               Hashtbl.update prev n ~f:(fun v ->
                   match v with
                   | None -> [ cur_node ]
                   | Some l -> cur_node :: l))
    done;
    let d =
        List.zip_exn directions [ e; e; e; e ]
        |> List.filter_map ~f:(fun x ->
               match Hashtbl.find dist x with
               | None -> None
               | Some d -> Some (d, x))
        |> List.min_elt ~compare:(fun (d, _) (d', _) -> Int.compare d d')
        |> Option.value_exn
    in
    (d, prev)

let get_paths prev e =
    let s = Hash_set.create (module Aoc.Pair) in
    let rec aux (n : Node.t) succ =
        let d, p = n in
        Hash_set.add s p;
        match Hashtbl.find prev n with
        | Some l -> List.iter l ~f:(fun x -> aux x n)
        | _ -> ()
    in
    aux e e;
    s

let rec count_seats paths = Hash_set.length paths

let create_graph m s =
    let valid_dirs (x, y) =
        if Aoc.Pair.equal (x, y) s then
          directions
        else (* neighbours but inverted since it represents the dir you can come from *)
          [ (0, 1); (0, -1); (1, 0); (-1, 0) ]
          |> List.zip_exn directions
          |> List.filter_map ~f:(fun (d, (dx, dy)) ->
                 if Map.mem m (x + dx, y + dy) then Some d else None)
    in
    let neighbours_dir (x, y) : Node.t list =
        [ (0, -1); (0, 1); (-1, 0); (1, 0) ]
        |> List.zip_exn directions
        |> List.filter_map ~f:(fun (dir, (dx, dy)) ->
               if Map.mem m (x + dx, y + dy) then
                 Some (dir, (x + dx, y + dy))
               else
                 None)
    in
    let tbl = Hashtbl.create (module Node) in
    Map.iter_keys m ~f:(fun (x, y) ->
        List.iter
          (valid_dirs (x, y))
          ~f:(fun d ->
            let node = (d, (x, y)) in
            let l = neighbours_dir (x, y) in
            Hashtbl.set tbl ~key:node ~data:l));
    tbl

let () =
    let m = Aoc.read_to_map "day16" (fun c -> not (Char.equal c '#')) in
    let full = Aoc.read_to_map "day16" (fun c -> true) in
    let s, e =
        Map.fold m
          ~init:((0, 0), (0, 0))
          ~f:(fun ~key ~data (s, e) ->
            match data with
            | 'S' -> (key, e)
            | 'E' -> (s, key)
            | _ -> (s, e))
    in
    let g = create_graph m s in
    let (d, e), prevs = djikstra g s e in
    let paths = get_paths prevs e in
    print_path full paths;
    Printf.printf "Part 1: %d\n" d;
    count_seats paths |> Printf.printf "Part 2: %d\n"
