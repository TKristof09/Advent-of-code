open Core

type t =
    | Key of int list
    | Lock of int list
[@@deriving show]

let parse l =
    let calc_height l =
        let a, b, c, d, e =
            List.fold (List.tl_exn l) ~init:(0, 0, 0, 0, 0) ~f:(fun (a, b, c, d, e) s ->
                let open Char in
                let a = if s.[0] = '#' then a + 1 else a in
                let b = if s.[1] = '#' then b + 1 else b in
                let c = if s.[2] = '#' then c + 1 else c in
                let d = if s.[3] = '#' then d + 1 else d in
                let e = if s.[4] = '#' then e + 1 else e in
                (a, b, c, d, e))
        in
        [ a; b; c; d; e ]
    in
    let it =
        IterLabels.map l ~f:(fun l ->
            if String.equal (List.hd_exn l) "#####" then
              let heights = calc_height l in
              Lock heights
            else
              let heights = calc_height (List.rev l) in
              Key heights)
    in
    ( IterLabels.filter it ~f:(function
        | Lock _ -> true
        | Key _ -> false),
      IterLabels.filter it ~f:(function
        | Lock _ -> false
        | Key _ -> true) )
    |> Tuple2.map ~f:Iter.to_list

let fits (lock, key) =
    let lock =
        match lock with
        | Lock l -> l
        | Key _ -> assert false
    in
    let key =
        match key with
        | Lock _ -> assert false
        | Key l -> l
    in
    List.for_all2_exn lock key ~f:(fun l k -> l + k <= 5)

let () =
    let locks, keys = Aoc.read_to_iter "day25" |> Aoc.split_iter ~on:String.is_empty |> parse in
    List.cartesian_product locks keys |> List.count ~f:fits |> Printf.printf "Part 1: %d\n"
