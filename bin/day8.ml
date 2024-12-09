open Core

let parse_input m =
    Map.fold m ~init:Char.Map.empty ~f:(fun ~key ~data acc ->
        Map.update acc data ~f:(function
          | None -> [ key ]
          | Some l -> key :: l))

let dist (x1, y1) (x2, y2) =
    Float.sqrt (Float.of_int (((x2 - x1) * (x2 - x1)) + ((y2 - y1) * (y2 - y1))))

let is_in_grid (x, y) (size_x, size_y) = 0 <= x && x < size_x && 0 <= y && y < size_y

let find_antinodes t1 t2 grid_size =
    let x1, y1 = t1 in
    let x2, y2 = t2 in
    let xt t = Float.of_int x1 +. (t *. Float.of_int (x2 - x1)) in
    let yt t = Float.of_int y1 +. (t *. Float.of_int (y2 - y1)) in
    let grid_max = Tuple2.uncurry max grid_size in
    [
      (xt (-1.), yt (-1.));
      (xt (1. /. 3.), yt (1. /. 3.));
      (xt (2. /. 3.), yt (2. /. 3.));
      (xt 2., yt 2.);
    ]
    @ List.init grid_max ~f:(fun i ->
          let i = Float.of_int (-i) in
          (xt i, yt i))
    @ List.init grid_max ~f:(fun i ->
          let i = Float.of_int (i + 1) in
          (xt i, yt i))
    |> List.filter_map ~f:(fun p ->
           let x, y = Tuple2.map p ~f:Int.of_float in
           if is_in_grid (x, y) grid_size && Float.is_integer (fst p) && Float.is_integer (snd p)
           then
             Some (x, y)
           else
             None)

let rec iter_pairs l f =
    match l with
    | [] -> ()
    | x :: t ->
        List.iter t ~f:(fun y -> f x y);
        iter_pairs t f

let fold_pairs lst ~init ~f =
    let rec aux rest acc =
        match rest with
        | [] -> acc
        | x :: t ->
            let acc' = List.fold t ~init:acc ~f:(fun acc y -> f acc x y) in
            aux t acc'
    in
    aux lst init

let print_map m =
    Map.iteri m ~f:(fun ~key ~data ->
        Printf.printf "%c -> %s\n" key ([%derive.show: Aoc.Pair.t list] data))

let () =
    let m = Aoc.read_to_map "day8" (fun c -> Char.is_alphanum c) |> parse_input in
    let input = Aoc.read_to_array "day8" in
    let grid_size = (String.length input.(0), Array.length input) in
    print_map m;
    Map.fold m
      ~init:(Set.empty (module Aoc.Pair))
      ~f:(fun ~key ~data acc ->
        fold_pairs data ~init:acc ~f:(fun acc' t1 t2 ->
            let a = find_antinodes t1 t2 grid_size in
            List.fold a ~init:acc' ~f:(fun s p -> Set.add s p)))
    |> Set.length
    |> Printf.printf "%d\n"
(*   Aoc.read_to_map "day8" (fun c -> Char.equal c '#') *)
(* |> Map.iter_keys ~f:(fun (x, y) -> Printf.printf "(%d, %d)\n" x y) *)

let () = Printf.printf "\n"
