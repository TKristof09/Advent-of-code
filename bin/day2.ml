open Core

type order =
    | Increasing
    | Decreasing

let remove_each lst =
    Sequence.range 0 (List.length lst)
    |> Sequence.map ~f:(fun i -> List.filteri lst ~f:(fun j _ -> i <> j))

let is_safe l =
    let start = List.hd_exn l in
    let l = List.tl_exn l in
    List.fold l ~init:(start, true, None) ~f:(fun (last, res, order) x ->
        match order with
        | None ->
            (* first element so we figure out the order *)
            let diff = x - last in
            let next_res = 1 <= abs diff && abs diff <= 3 in
            (x, next_res && res, if diff < 0 then Some Decreasing else Some Increasing)
        | Some Decreasing ->
            let diff = x - last in
            let next_res = 1 <= abs diff && abs diff <= 3 in
            (x, next_res && res && diff < 0, order)
        | Some Increasing ->
            let diff = x - last in
            let next_res = 1 <= abs diff && abs diff <= 3 in
            (x, next_res && res && diff > 0, order))
    |> Tuple3.get2

let () =
    Aoc.read_to_array_filtered "day2"
    |> Array.map ~f:(fun l -> String.split l ~on:' ' |> List.map ~f:Int.of_string)
    |> Array.filter ~f:(fun r ->
           if is_safe r then
             true
           else
             remove_each r
             |> Sequence.fold_until ~init:false
                  ~f:(fun _ rr ->
                    if is_safe rr then
                      Stop true
                    else
                      Continue false)
                  ~finish:(fun _ -> false))
    |> Array.length
    |> Printf.printf "%d\n"
