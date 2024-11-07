open Core

module Pair = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp, show]
  end

  include T
  include Comparable.Make_plain (T)
end

type sketch = {
      start_pos: int * int
    ; map: (int * int) list Map.M(Pair).t
  }

let show_sketch sketch =
    Printf.sprintf "Start: %s\n%s"
      ([%derive.show: int * int] sketch.start_pos)
      ([%derive.show: (Pair.t * (int * int) list) list]
         (Map.to_alist sketch.map ~key_order:`Increasing) )

let empty_sketch = { start_pos= (-1, -1); map= Map.empty (module Pair) }

(* y axis points down *)
let parse_graph lines =
    Array.foldi lines ~init:empty_sketch ~f:(fun y sketch line ->
        String.foldi line ~init:sketch ~f:(fun x sketch c ->
            match c with
            | '|' ->
                {
                  sketch with
                  map= Map.add_exn sketch.map ~key:(x, y) ~data:[(x, y - 1); (x, y + 1)]
                }
            | '-' ->
                {
                  sketch with
                  map= Map.add_exn sketch.map ~key:(x, y) ~data:[(x - 1, y); (x + 1, y)]
                }
            | 'L' ->
                {
                  sketch with
                  map= Map.add_exn sketch.map ~key:(x, y) ~data:[(x, y - 1); (x + 1, y)]
                }
            | 'J' ->
                {
                  sketch with
                  map= Map.add_exn sketch.map ~key:(x, y) ~data:[(x, y - 1); (x - 1, y)]
                }
            | '7' ->
                {
                  sketch with
                  map= Map.add_exn sketch.map ~key:(x, y) ~data:[(x, y + 1); (x - 1, y)]
                }
            | 'F' ->
                {
                  sketch with
                  map= Map.add_exn sketch.map ~key:(x, y) ~data:[(x, y + 1); (x + 1, y)]
                }
            | 'S' -> { sketch with start_pos= (x, y) }
            | _ -> sketch ) )

let resolve_s_connections sketch =
    let x, y = sketch.start_pos in
    let neighbours = [(x, y - 1); (x, y + 1); (x - 1, y); (x + 1, y)] in
    let connections =
        List.filter neighbours ~f:(fun p ->
            match Map.find sketch.map p with
            | None -> false
            | Some connections -> List.mem connections sketch.start_pos ~equal:Pair.equal )
    in
    { sketch with map= Map.add_exn sketch.map ~key:sketch.start_pos ~data:connections }

let get_loop sketch =
    let rec aux cur next =
        if Pair.equal next sketch.start_pos then
          [cur]
        else
          match Map.find sketch.map next with
          | None -> failwith "how did this happen"
          | Some [c; next_next] when Pair.equal c cur -> cur :: aux next next_next
          | Some [next_next; c] when Pair.equal c cur -> cur :: aux next next_next
          | _ -> failwith "this should be impossible"
    in
    let starts = Map.find_exn sketch.map sketch.start_pos in
    aux sketch.start_pos (List.hd_exn starts)
(* doesn't matter which way we start *)

let loop_dist sketch =
    let loop = get_loop sketch in
    let loop_len = List.length loop in
    List.mapi loop ~f:(fun i _ ->
        let d = min i (loop_len - i) in
        d )
    |> List.max_elt ~compare:Int.compare

let loop_to_polygon input_array sketch loop =
    List.filter loop ~f:(fun (x, y) ->
        match input_array.(y).[x] with
        | 'L' -> true
        | 'J' -> true
        | '7' -> true
        | 'F' -> true
        | 'S' -> (
            match Map.find_exn sketch.map (x, y) with
            | [(x1, y1); (x2, y2)] when x1 = x2 || y1 = y2 -> false
            | _ -> true )
        | _ -> false )

(* Shoelace formula https://en.wikipedia.org/wiki/Shoelace_formula *)
let find_area polygon =
    let sum =
        List.fold2_exn polygon
          (List.tl_exn polygon @ [List.hd_exn polygon])
          ~init:0
          ~f:(fun acc (x1, y1) (x2, y2) -> (x1 * y2) - (x2 * y1) + acc)
    in
    Float.abs @@ (Int.to_float sum /. 2.0)

(* Pick's theorem https://en.wikipedia.org/wiki/Pick%27s_theorem *)
(* need to use floats otherwise it doesn't work with odd length polygons or odd sum above *)
let find_integer_points_inside polygon area =
    let b = List.length polygon in
    area +. 1.0 -. (Int.to_float b /. 2.0)

let show_option opt = [%derive.show: int option] opt

let show_loop l = [%derive.show: (int * int) list] l

let () =
    let inp = Aoc.read_to_array_filtered "day10" in
    let sketch = inp |> parse_graph |> resolve_s_connections in
    let loop = get_loop sketch in
    let poly = loop_to_polygon inp sketch loop in
    Printf.printf "%s\n" (show_loop poly) ;
    poly |> find_area |> find_integer_points_inside loop |> Printf.printf "%f\n"
