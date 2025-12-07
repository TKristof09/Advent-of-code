open Core

let parse l =
    let start = String.index_exn (List.hd_exn l) 'S' in
    let splitters =
        List.map (List.tl_exn l) ~f:(fun s ->
            String.foldi s ~init:Int.Set.empty ~f:(fun i res c ->
                if Char.equal c '^' then
                  Set.add res i
                else
                  res))
        |> List.filter ~f:(fun s -> not (Set.is_empty s))
    in
    (start, splitters)

let inp = Aoc.read_to_list "day7"

let part1 () =
    let start, splitters = parse inp in
    (* Printf.printf "Start: %d\n" start; *)
    (* Printf.printf "Splitters: %s\n" *)
    (*   ([%derive.show: int list list] (List.map splitters ~f:Set.to_list)); *)
    let rays = Int.Set.singleton start in
    List.fold splitters ~init:(0, rays) ~f:(fun (num_splits, rays) splits ->
        (* Printf.printf "Rays: %s\n" ([%derive.show: int list] (Set.to_list rays)); *)
        Set.fold rays ~init:(num_splits, Int.Set.empty) ~f:(fun (num_splits, res) ray ->
            if Set.mem splits ray then
              let s = Set.add res (ray - 1) in
              (num_splits + 1, Set.add s (ray + 1))
            else
              (num_splits, Set.add res ray)))
    |> fst

let part2 () =
    let start, splitters = parse inp in
    (* Printf.printf "Start: %d\n" start; *)
    (* Printf.printf "Splitters: %s\n" *)
    (*   ([%derive.show: int list list] (List.map splitters ~f:Set.to_list)); *)
    let rays = Int.Map.singleton start 1 in
    let rays =
        List.fold splitters ~init:rays ~f:(fun rays splits ->
            (* Printf.printf "Rays: %s\n" ([%derive.show: (int * int) list] (Map.to_alist rays)); *)
            (* Printf.printf "Splitters: %s\n" ([%derive.show: int list] (Set.to_list splits)); *)
            Map.fold rays ~init:Int.Map.empty ~f:(fun ~key:ray ~data:incoming res ->
                if Set.mem splits ray then
                  let m =
                      Map.update res (ray - 1) ~f:(fun v ->
                          match v with
                          | None -> incoming
                          | Some n -> n + incoming)
                  in
                  Map.update m (ray + 1) ~f:(fun v ->
                      match v with
                      | None -> incoming
                      | Some n -> n + incoming)
                else
                  Map.update res ray ~f:(fun v ->
                      match v with
                      | None -> incoming
                      | Some n -> n + incoming)))
    in
    (* Printf.printf "Rays: %s\n" ([%derive.show: (int * int) list] (Map.to_alist rays)); *)
    rays |> Map.sum (module Int) ~f:Fun.id

let () =
    part1 () |> Printf.printf "Part 1: %d tachyon splits\n";
    part2 () |> Printf.printf "Part 2: %d timelines explored\n"

let () =
    Printf.printf "Part 1: ";
    Aoc.time_fn part1;
    Printf.printf "Part 2: ";
    Aoc.time_fn part2
