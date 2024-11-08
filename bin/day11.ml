open Core

module Pair = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp, show]
  end

  include T
  include Comparable.Make_plain (T)
end

let parse_galaxies lines =
    let x_max = String.length lines.(0) in
    let y_max = Array.length lines in
    let coords =
        Array.foldi lines ~init:[] ~f:(fun y acc s ->
            String.foldi s ~init:acc ~f:(fun x acc c ->
                if Char.equal c '#' then (x, y) :: acc else acc ) )
    in
    let coords = List.sort coords ~compare:(fun (x, _) (x', _) -> Int.compare x x') in
    let coords, _, _ =
        List.fold coords ~init:([], 0, 0) ~f:(fun (acc, last, extra) (x, y) ->
            let diff = max (x - last - 1) 0 in
            let diff = diff * 999999 in
            let new_x = x + diff + extra in
            ((new_x, y) :: acc, x, extra + diff) )
    in
    let coords = List.sort coords ~compare:(fun (_, y) (_, y') -> Int.compare y y') in
    let coords, _, _ =
        List.fold coords ~init:([], 0, 0) ~f:(fun (acc, last, extra) (x, y) ->
            let diff = max (y - last - 1) 0 in
            let diff = diff * 999999 in
            let new_y = y + diff + extra in
            ((x, new_y) :: acc, y, extra + diff) )
    in
    coords

let list_pairs l1 l2 =
    let rec aux l l' =
        match l with
        | [] -> []
        | x :: t ->
            List.fold (List.tl_exn l')
              ~init:(aux t (List.tl_exn l'))
              ~f:(fun acc x' -> (x, x') :: acc)
    in
    aux l1 l2

let calc_res galaxies =
    let manhattan (x, y) (x', y') = abs (x' - x) + abs (y' - y) in
    let pairs = list_pairs galaxies galaxies in
    (* Printf.printf "%d\n" (List.length pairs); *)
    pairs |> List.fold ~init:0 ~f:(fun acc (g, g') -> acc + manhattan g g')

(* let () = *)
(*     list_pairs [1;2;3] [1;2;3] *)
(*     |> [%derive.show: (int * int) list] *)
(*     |> Printf.printf "%s\n" *)

let () =
    let galaxies = Aoc.read_to_array_filtered "day11" |> parse_galaxies in
    (* galaxies *)
    (* |> [%derive.show: (int * int) list] *)
    (* |> Printf.printf "%s\n"; *)
    Printf.printf "%d\n" (calc_res galaxies)
