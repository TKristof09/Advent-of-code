open Core

let () =
    let left, right =
        Aoc.read_to_list_filtered "day1"
        |> List.fold ~init:([], []) ~f:(fun (l1, l2) l ->
               match
                 String.split l ~on:' ' |> List.filter ~f:(fun x -> not (String.is_empty x))
               with
               | [ a; b ] -> (Int.of_string a :: l1, Int.of_string b :: l2)
               | _ -> failwith "Impossible")
        |> Tuple2.map ~f:(List.sort ~compare:Int.compare)
    in
    let m =
        List.fold right ~init:Int.Map.empty ~f:(fun acc b ->
            Map.update acc b ~f:(fun v ->
                match v with
                | Some x -> x + 1
                | None -> 1))
    in
    List.fold left ~init:0 ~f:(fun acc a ->
        match Map.find m a with
        | Some x -> acc + (a * x)
        | None -> acc)
    |> Printf.printf "%d\n"
