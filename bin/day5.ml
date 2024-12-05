open Core

let parse_rules input =
    List.fold input ~init:Int.Map.empty ~f:(fun acc line ->
        let x, y = String.lsplit2_exn line ~on:'|' |> Tuple2.map ~f:Int.of_string in
        Map.update acc y ~f:(fun v ->
            match v with
            | None -> [ x ]
            | Some l -> x :: l))

let parse_order line = String.split line ~on:',' |> List.map ~f:Int.of_string

let check_update order rules =
    List.fold order ~init:(true, []) ~f:(fun (acc, already_done) n ->
        let ok =
            Map.find rules n
            |> Option.value ~default:[]
            |> List.filter ~f:(fun x -> List.mem order x ~equal:Int.equal)
            |> List.for_all ~f:(fun x -> List.mem already_done x ~equal:Int.equal)
        in
        (ok && acc, n :: already_done))
    |> fst

let reorder order rules =
    let compare x1 x2 =
        if x1 = x2 then
          0
        else
          let before_x1 = Map.find rules x1 |> Option.value ~default:[] in
          let before_x2 = Map.find rules x1 |> Option.value ~default:[] in
          let x2_lt_x1 = List.mem before_x1 x2 ~equal:Int.equal in
          let x1_lt_x2 = List.mem before_x2 x1 ~equal:Int.equal in
          let are_related = x2_lt_x1 || x1_lt_x2 in
          if not are_related then 0 else if x1_lt_x2 then -1 else 1
    in
    List.sort order ~compare

let () =
    let input = Aoc.read_to_list "day5" in
    let rules, order =
        List.split_n input (fst @@ List.findi_exn input ~f:(fun _ x -> String.is_empty x))
    in
    let order = List.tl_exn order |> List.map ~f:parse_order in
    let rules = parse_rules rules in
    List.filter_map order ~f:(fun order ->
        if check_update order rules then None else Some (reorder order rules))
    (* |> List.iter ~f:(fun order -> Printf.printf "%s\n" ([%derive.show: int list] order)) *)
    |> List.map ~f:(fun order -> List.nth_exn order (List.length order / 2))
    |> List.fold ~init:0 ~f:Int.( + )
    |> Printf.printf "%d\n"
