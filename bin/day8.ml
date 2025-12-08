open Core

module Triplet = struct
  module T = struct
    type t = int * int * int [@@deriving compare, sexp, hash, show]
  end

  include T
  include Comparable.Make_plain (T)
  include Hashable.Make_plain (T)

  let of_list l : t =
      match l with
      | [ x; y; z ] -> (x, y, z)
      | _ -> failwith "Invalid argument length"

  let map ((x, y, z) : t) ~f : t = (f x, f y, f z)
end

let parse arr =
    Array.map arr ~f:(fun s ->
        String.split s ~on:',' |> List.map ~f:Int.of_string |> Triplet.of_list)

let distance p p' =
    let x, y, z = p in
    let x', y', z' = p' in
    let dx = x' - x in
    let dy = y' - y in
    let dz = z' - z in
    (dx * dx) + (dy * dy) + (dz * dz)

let connected_comps size pairs =
    let adjacency = Hashtbl.create ~size (module Triplet) in
    IterLabels.iter pairs ~f:(fun (p, p') ->
        Hashtbl.update adjacency p ~f:(fun neighbours ->
            match neighbours with
            | None -> [ p' ]
            | Some l -> p' :: l);
        Hashtbl.update adjacency p' ~f:(fun neighbours ->
            match neighbours with
            | None -> [ p ]
            | Some l -> p :: l));
    let rec dfs visited stack =
        match stack with
        | [] -> visited
        | h :: t ->
            if Set.mem visited h then
              dfs visited t
            else
              let neighbours = Hashtbl.find_exn adjacency h in
              dfs (Set.add visited h) (neighbours @ stack)
    in
    let rec loop remaining acc =
        match Set.choose remaining with
        | None -> acc
        | Some x ->
            let comp = dfs (Set.empty (module Triplet)) [ x ] in
            loop (Set.diff remaining comp) (comp :: acc)
    in
    loop (Set.of_hashtbl_keys (module Triplet) adjacency) []

let is_complete size adjacency =
    let visited = Hashtbl.create (module Int) in
    let rec dfs stack =
        match stack with
        | [] -> ()
        | p :: t -> (
            match Hashtbl.add visited ~key:p ~data:() with
            | `Ok ->
                let neighbours = Hashtbl.find_exn adjacency p in
                dfs (neighbours @ stack)
            | `Duplicate -> dfs t)
    in
    dfs [ Hashtbl.choose_exn adjacency |> fst ];
    Hashtbl.length visited = size

let inp = Aoc.read_to_array "day8"
let grid_size = 300

let print_hashtbl htbl =
    Hashtbl.iteri htbl ~f:(fun ~key ~data ->
        Printf.printf "%s -> %s\n" (Triplet.show key) ([%derive.show: Triplet.t list] data))

let part1 num_connections =
    let points = parse inp in
    let pairs =
        Iter.int_range ~start:0 ~stop:(Array.length points - 1)
        |> Iter.diagonal
        |> IterLabels.map ~f:(fun (i, j) -> (i, j, distance points.(i) points.(j)))
        |> IterLabels.sort ~cmp:(fun (_, _, d) (_, _, d') -> Int.compare d d')
        |> Iter.take num_connections
        |> IterLabels.map ~f:(fun (i, j, d) -> (points.(i), points.(j)))
    in
    (* pairs |> Iter.to_string [%derive.show: Triplet.t * Triplet.t] |> Printf.printf "%s\n"; *)
    let circuits =
        pairs
        |> connected_comps num_connections
        |> List.sort ~compare:(fun comp comp' -> Int.compare (Set.length comp') (Set.length comp))
    in
    (* circuits *)
    (* |> List.map ~f:Set.to_list *)
    (* |> [%derive.show: Triplet.t list list] *)
    (* |> Printf.printf "%s\n"; *)
    List.take circuits 3 |> List.fold ~init:1 ~f:(fun res s -> res * Set.length s)

let part2 () =
    let points = parse inp in
    let pairs =
        Iter.int_range ~start:0 ~stop:(Array.length points - 1)
        |> Iter.diagonal
        |> IterLabels.map ~f:(fun (i, j) -> (i, j, distance points.(i) points.(j)))
        |> IterLabels.sort ~cmp:(fun (_, _, d) (_, _, d') -> Int.compare d d')
        (* |> IterLabels.map ~f:(fun (i, j, d) -> (points.(i), points.(j))) *)
        |> IterLabels.map ~f:(fun (i, j, _) -> (i, j))
    in
    let adjacency = Hashtbl.create ~size:(Array.length points) (module Int) in
    let last_connection =
        pairs
        |> IterLabels.find_pred_exn ~f:(fun (i, j) ->
            Hashtbl.update adjacency i ~f:(fun neighbours ->
                match neighbours with
                | None -> [ j ]
                | Some l -> j :: l);
            Hashtbl.update adjacency j ~f:(fun neighbours ->
                match neighbours with
                | None -> [ i ]
                | Some l -> i :: l);
            is_complete (Array.length points) adjacency)
        |> Tuple2.map ~f:(Array.get points)
    in
    (* last_connection |> [%derive.show: Triplet.t * Triplet.t] |> Printf.printf "%s\n"; *)
    let (x, _, _), (x', _, _) = last_connection in
    x * x'

let () =
    part1 1000 |> Printf.printf "Part 1: %d\n";
    part2 () |> Printf.printf "Part 2: %d\n"

let () =
    Printf.printf "Part 1: ";
    Aoc.time_fn (fun () -> part1 1000);
    Printf.printf "Part 2: ";
    Aoc.time_fn part2
