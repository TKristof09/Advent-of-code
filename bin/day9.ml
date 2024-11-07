open Core

let parse_histories lines =
    List.map lines ~f:(fun s -> String.split s ~on:' ' |> List.map ~f:Int.of_string)

let rec map2 l1 l2 f =
    match (l1, l2) with
    | [], _ -> []
    | _, [] -> []
    | x1 :: t1, x2 :: t2 -> f x1 x2 :: map2 t1 t2 f

let predict history =
    let rec derive l =
        if List.for_all l ~f:(fun x -> x = 0) then
          0
        else
          let d = map2 l (List.tl_exn l) (fun x y -> y - x) in
          List.last_exn l + derive d
    in
    derive history

let predict_back history =
    let rec derive l =
        if List.for_all l ~f:(fun x -> x = 0) then
          0
        else
          let d = map2 l (List.tl_exn l) (fun x y -> y - x) in
          List.hd_exn l - derive d
    in
    derive history

let () =
    Aoc.read_to_list_filtered "day9"
    |> parse_histories
    (* |> List.fold ~init:0 ~f:(fun acc history -> let d = predict history in Printf.printf "%d\n" d; acc + d) *)
    |> List.fold ~init:0 ~f:(fun acc history ->
           let d = predict_back history in
           Printf.printf "%d\n" d ; acc + d )
    |> Printf.printf "%d\n"
