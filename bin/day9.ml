open Core

type entry =
    | Block of int
    | File of int * int

let show_entry e =
    match e with
    | Block size -> Printf.sprintf "(BLOCK %d)" size
    | File (id, size) -> Printf.sprintf "(FILE %d: %d)" id size

let parse_line line =
    String.foldi line ~init:[] ~f:(fun i acc c ->
        let n = Char.to_string c |> Int.of_string in
        let e = if i mod 2 = 0 then File (i / 2, n) else Block n in
        e :: acc)
    |> List.rev

let print_fs l =
    List.iter l ~f:(fun e -> Printf.printf "%s; " (show_entry e));
    Printf.printf "\n"

let file_map fs =
    List.fold fs ~init:Int.Map.empty ~f:(fun acc e ->
        match e with
        | Block _ -> acc
        | File (id, size) -> Map.set acc ~key:id ~data:size)

let clean_fs1 fs =
    (* print_fs fs; *)
    let move_files s files =
        let s = ref s in
        let m = ref files in
        let moved = ref [] in
        while !s > 0 do
          match Map.max_elt !m with
          | None -> s := 0
          | Some (id, size) ->
              if size > !s then (
                moved := File (id, !s) :: !moved;
                m := Map.set !m ~key:id ~data:(size - !s);
                s := 0)
              else if size < !s then (
                moved := File (id, size) :: !moved;
                m := Map.remove !m id;
                s := !s - size)
              else (
                moved := File (id, !s) :: !moved;
                m := Map.remove !m id;
                s := 0)
        done;
        (!moved, !m)
    in
    let fix_last l =
        match l with
        | File (id1, s1) :: File (id2, s2) :: t when id1 = id2 -> File (id1, s1 + s2) :: t
        | _ -> l
    in
    let files = file_map fs in
    List.fold_until fs ~init:([], files)
      ~f:(fun (acc, files) e ->
        if Map.is_empty files then
          Stop acc
        else
          match e with
          | Block s ->
              let moved, new_files = move_files s files in
              Continue (moved @ acc, new_files)
          | File (id, _) -> (
              match Map.find files id with
              | None -> Continue (acc, Map.remove files id)
              | Some size -> Continue (File (id, size) :: acc, Map.remove files id)))
      ~finish:(fun (fs, _) -> fs)
    |> fix_last
    |> List.rev

let clean_fs2 fs =
    (* print_fs fs; *)
    let rec remove_file fs id =
        match fs with
        | [] -> []
        | File (id', size) :: t when id' = id -> Block size :: t
        | h :: t -> h :: remove_file t id
    in

    let rec move_file fs id size =
        match fs with
        | [] -> []
        | Block s :: t ->
            if s > size then
              File (id, size) :: Block (s - size) :: remove_file t id
            else if s < size then
              Block s :: move_file t id size
            else
              File (id, size) :: remove_file t id
        | (File (id', _) as f) :: t when id' <> id -> f :: move_file t id size
        | File (id', _) :: t (* when id' = id *) -> fs
    in
    let fs =
        List.fold (List.rev fs) ~init:fs ~f:(fun fs e ->
            match e with
            | Block _ -> fs
            | File (id, size) -> move_file fs id size)
        (* |> fix_last *)
    in

    (* print_fs fs; *)
    fs

let checksum fs =
    List.fold fs ~init:(0, 0) ~f:(fun (acc, i) e ->
        match e with
        | Block size -> (acc, i + size)
        | File (id, size) ->
            let sum = Sequence.range i (i + size) |> Sequence.fold ~init:0 ~f:Int.( + ) in
            (acc + (sum * id), i + size))
    |> Tuple2.get1

let () =
    Aoc.read_to_list "day9"
    |> List.hd_exn
    |> parse_line
    |> clean_fs1
    |> checksum
    |> Printf.printf "PART 1: %d\n"

let () =
    Aoc.read_to_list "day9"
    |> List.hd_exn
    |> parse_line
    |> clean_fs2
    |> checksum
    |> Printf.printf "PART 2: %d\n"

let () = Printf.printf "\n"
