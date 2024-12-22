open Core

exception Break

let number_grid =
    Map.empty (module Char)
    |> Map.set ~key:'0' ~data:(1, 0)
    |> Map.set ~key:'A' ~data:(2, 0)
    |> Map.set ~key:'1' ~data:(0, 1)
    |> Map.set ~key:'2' ~data:(1, 1)
    |> Map.set ~key:'3' ~data:(2, 1)
    |> Map.set ~key:'4' ~data:(0, 2)
    |> Map.set ~key:'5' ~data:(1, 2)
    |> Map.set ~key:'6' ~data:(2, 2)
    |> Map.set ~key:'7' ~data:(0, 3)
    |> Map.set ~key:'8' ~data:(1, 3)
    |> Map.set ~key:'9' ~data:(2, 3)

let direction_grid =
    Map.empty (module Char)
    |> Map.set ~key:'<' ~data:(0, 0)
    |> Map.set ~key:'v' ~data:(1, 0)
    |> Map.set ~key:'>' ~data:(2, 0)
    |> Map.set ~key:'^' ~data:(1, 1)
    |> Map.set ~key:'A' ~data:(2, 1)

let get_neighbours (x, y) sx sy forbidden =
    [ (0, -1); (0, 1); (-1, 0); (1, 0) ]
    |> List.filter_map ~f:(fun (dx, dy) ->
           let px, py = (x + dx, y + dy) in
           if 0 <= px && px < sx && 0 <= py && py < sy && not (Aoc.Pair.equal (px, py) forbidden)
           then
             Some (px, py)
           else
             None)

let coords sx sy =
    let l = List.range ~stop:`inclusive 0 sx in
    List.cartesian_product l (List.range ~stop:`inclusive 0 sy)

let djikstra ~start ~end_ sx sy forbidden =
    let create_tbl get_data =
        let tbl =
            Hashtbl.create_mapped (module Aoc.Pair) ~get_key:(fun x -> x) ~get_data (coords sx sy)
        in
        match tbl with
        | `Ok tbl -> tbl
        | `Duplicate_keys _ -> failwith "Error"
    in
    let dist = create_tbl (fun n -> if Aoc.Pair.equal n start then 0 else Int.max_value) in
    let prev = create_tbl (fun n -> if Aoc.Pair.equal n start then [] else []) in
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
            get_neighbours (x, y) sx sy forbidden
            |> List.iter ~f:(fun n ->
                   let old_dist = Hashtbl.find_exn dist n in
                   let d = cur_dist + 1 in
                   if d < old_dist then (
                     Hashtbl.set dist ~key:n ~data:d;
                     Hashtbl.set prev ~key:n ~data:[ cur_node ])
                   else if d = old_dist then
                     Hashtbl.update prev n ~f:(fun v ->
                         match v with
                         | None -> [ cur_node ]
                         | Some l -> cur_node :: l))
          done
        with
        | Break -> ()
    in
    (dist, prev)

let find_path ~start ~end_ prev =
    let rec aux n path acc =
        let nx, ny = n in
        match Hashtbl.find prev n with
        | Some [ x ] when Aoc.Pair.equal x start ->
            (* List.rev ((0, 0) :: (nx - fst start, ny - snd start) :: path) :: acc *)
            (((nx - fst start, ny - snd start) :: path) @ [ (0, 0) ]) :: acc
        | Some l ->
            List.fold l ~init:acc ~f:(fun acc (x, y) -> aux (x, y) ((nx - x, ny - y) :: path) acc)
        | None -> failwith "No path"
    in
    aux end_ [] []

let find_number_path ~start ~end_ =
    if Char.equal start end_ then
      [ [ (0, 0) ] ]
    else
      let sx, sy = (3, 4) in
      let start = Map.find_exn number_grid start in
      let end_ = Map.find_exn number_grid end_ in
      let _, prev = djikstra ~start ~end_ sx sy (0, 0) in
      find_path ~start ~end_ prev

let find_direction_path ~start ~end_ =
    if Char.equal start end_ then
      [ [ (0, 0) ] ]
    else
      let sx, sy = (3, 2) in
      let start = Map.find_exn direction_grid start in
      let end_ = Map.find_exn direction_grid end_ in
      let _, prev = djikstra ~start ~end_ sx sy (0, 1) in
      find_path ~start ~end_ prev

let combine paths paths' =
    if List.is_empty paths' then
      paths
    else if List.is_empty paths then
      paths'
    else
      List.concat_map paths ~f:(fun path ->
          List.fold paths' ~init:[] ~f:(fun acc path' -> (path ^ path') :: acc))

let path_to_string p =
    List.map p ~f:(fun (dx, dy) ->
        match (dx, dy) with
        | 1, 0 -> '>'
        | -1, 0 -> '<'
        | 0, 1 -> '^'
        | 0, -1 -> 'v'
        | 0, 0 -> 'A'
        | _ -> failwith "Error")
    |> String.of_char_list

module CharPair = struct
  type t = char * char [@@deriving hash, compare, sexp]
end

let numpad =
    let keys = [ '0'; 'A'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ] in
    let tbl = Hashtbl.create (module CharPair) in
    List.cartesian_product keys keys
    |> List.iter ~f:(fun (a, b) ->
           let p = find_number_path ~start:a ~end_:b |> List.map ~f:path_to_string in
           Hashtbl.set tbl ~key:(a, b) ~data:p);
    tbl

let dirgrid =
    let keys = [ '<'; 'v'; '>'; '^'; 'A' ] in
    let tbl = Hashtbl.create (module CharPair) in
    List.cartesian_product keys keys
    |> List.iter ~f:(fun (a, b) ->
           let p = find_direction_path ~start:a ~end_:b |> List.map ~f:path_to_string in
           Hashtbl.set tbl ~key:(a, b) ~data:p);
    tbl

let get_num_sequence s =
    let s = String.to_list s in
    let rec aux acc = function
        | []
        | [ _ ] ->
            acc
        | a :: (b :: rest as t) -> aux (combine acc (Hashtbl.find_exn numpad (a, b))) t
    in
    Iter.of_list (aux [] ('A' :: s))

let get_dir_sequence s =
    let s = String.to_list s in
    let rec aux acc = function
        | []
        | [ _ ] ->
            acc
        | a :: (b :: rest as t) -> aux (combine acc (Hashtbl.find_exn dirgrid (a, b))) t
    in
    Iter.of_list (aux [] ('A' :: s))

module Entry = struct
  type t = int * string [@@deriving compare, hash, sexp]
end

let cache = Hashtbl.create (module Entry)

let rec find_shortest depth keys =
    if depth = 0 then
      String.length keys
    else
      match Hashtbl.find cache (depth, keys) with
      | Some len -> len
      | None ->
          let l = String.chop_suffix_if_exists keys ~suffix:"A" |> String.split ~on:'A' in
          let res =
              List.fold l ~init:0 ~f:(fun acc subkey ->
                  let subkey = String.append subkey "A" in
                  let res =
                      get_dir_sequence subkey
                      |> IterLabels.fold ~init:Int.max_value ~f:(fun acc seq ->
                             let x = find_shortest (depth - 1) seq in
                             if x < acc then
                               x
                             else
                               acc)
                  in
                  acc + res)
          in
          Hashtbl.set cache ~key:(depth, keys) ~data:res;
          res

let () =
    let input = Aoc.read_to_list "day21" in
    input
    |> List.sum
         (module Int)
         ~f:(fun code ->
           let len =
               code
               |> get_num_sequence
               |> IterLabels.map ~f:(find_shortest 2)
               |> IterLabels.min_exn ~lt:Int.( < )
           in
           let i = String.drop_suffix code 1 |> Int.of_string in
           Printf.printf "%d -- %d\n" i len;
           i * len)
    |> Printf.printf "Part 1: %d\n";

    input
    |> List.sum
         (module Int)
         ~f:(fun code ->
           let len =
               code
               |> get_num_sequence
               |> IterLabels.map ~f:(find_shortest 25)
               |> IterLabels.min_exn ~lt:Int.( < )
           in
           let i = String.drop_suffix code 1 |> Int.of_string in
           Printf.printf "%d -- %d\n" i len;
           i * len)
    |> Printf.printf "Part 2: %d\n"
