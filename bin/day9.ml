open Core

let is_inside (vert_edges, horiz_edges) (x, y) =
    if Hash_set.mem horiz_edges (x, y) then
      true
    else
      match
        Hashtbl.find vert_edges y
      with
      | None -> false
      | Some s ->
          if Set.mem s x then
            true
          else
            let num_passings = Set.count s ~f:(fun x' -> x' <= x) in
            num_passings mod 2 = 1
let get_borders reds =
    (* y -> borders on that row *)
    let vert_edges = Hashtbl.create (module Int) in
    let horiz_edges = Hash_set.create (module Aoc.Pair) in
    let add_to_table (x, y) (x', y') =
        if x = x' then
          let start, stop = if y > y' then (y', y - 1) else (y, y' - 1) in
          Iter.int_range ~start ~stop
          |> IterLabels.iter ~f:(fun gy ->
              Hashtbl.update vert_edges gy ~f:(fun d ->
                  match d with
                  | None -> Set.singleton (module Int) x
                  | Some s -> Set.add s x))
        else
          Iter.int_range ~start:(min x x') ~stop:(max x x')
          |> IterLabels.iter ~f:(fun x -> Hash_set.add horiz_edges (x, y))
    in
    let last =
        List.fold (List.tl_exn reds) ~init:(List.hd_exn reds) ~f:(fun last cur ->
            add_to_table last cur;
            cur)
    in
    add_to_table last (List.hd_exn reds);
    (vert_edges, horiz_edges)

let print_map reds borders =
    let dimy =
        (List.max_elt reds ~compare:(fun (_, y) (_, y') -> Int.compare y y')
        |> Option.value_exn
        |> snd)
        + 2
    in
    let dimx =
        (List.max_elt reds ~compare:(fun (x, _) (x', _) -> Int.compare x x')
        |> Option.value_exn
        |> fst)
        + 2
    in
    let arr = Array.make_matrix ~dimx ~dimy '.' in
    (* Hash_set.iter greens ~f:(fun (x, y) -> arr.(y).(x) <- 'X'); *)
    Iter.product
      (Iter.int_range ~start:0 ~stop:(dimx - 1))
      (Iter.int_range ~start:0 ~stop:(dimy - 1))
    |> IterLabels.iter ~f:(fun (x, y) -> if is_inside borders (x, y) then arr.(x).(y) <- 'X');
    List.iter reds ~f:(fun (x, y) -> arr.(x).(y) <- '#');
    (* List.iter reds ~f:(fun (x, y) -> arr.(y).(x) <- '#'); *)
    arr
    |> Array.transpose_exn
    |> Array.iter ~f:(fun chars -> String.of_array chars |> Printf.printf "%s\n")

let gen_rectangle_borders (x, y) (x', y') =
    let x_range = Iter.int_range ~start:(min x x' + 1) ~stop:(max x x' - 1) in
    let y_range = Iter.int_range ~start:(min y y' + 1) ~stop:(max y y' - 1) in
    let e1 = IterLabels.map x_range ~f:(fun x -> (x, y)) in
    let e2 = IterLabels.map x_range ~f:(fun x -> (x, y')) in
    let e3 = IterLabels.map y_range ~f:(fun y -> (x, y)) in
    let e4 = IterLabels.map y_range ~f:(fun y -> (x', y)) in
    Iter.append_l [ e1; e2; e3; e4 ]

let inp = Aoc.read_to_list "day9"

let part1 () =
    inp
    |> List.map ~f:(fun s -> String.lsplit2_exn s ~on:',' |> Tuple2.map ~f:Int.of_string)
    |> Iter.diagonal_l
    |> IterLabels.map ~f:(fun ((x, y), (x', y')) ->
        let dx = abs (x' - x) + 1 in
        let dy = abs (y' - y) + 1 in
        dx * dy)
    |> Iter.sort ~cmp:(fun a b -> Int.compare b a)
    |> Iter.head_exn
    |> Printf.printf "Part 1: %d\n"

let part2 () =
    let reds =
        inp |> List.map ~f:(fun s -> String.lsplit2_exn s ~on:',' |> Tuple2.map ~f:Int.of_string)
    in
    let borders = get_borders reds in
    reds
    |> Iter.diagonal_l
    |> IterLabels.map ~f:(fun ((x, y), (x', y')) ->
        let dx = abs (x' - x) + 1 in
        let dy = abs (y' - y) + 1 in
        ((x, y), (x', y'), dx * dy))
    |> Iter.sort ~cmp:(fun (_, _, a) (_, _, b) -> Int.compare b a)
    |> IterLabels.drop_while ~f:(fun (p, p', _) ->
        let points = gen_rectangle_borders p p' in
        IterLabels.exists points ~f:(fun p -> not (is_inside borders p)))
    |> Iter.head_exn
    |> Tuple3.get3
    |> Printf.printf "Part 2: %d\n"

let () =
    part1 ();
    part2 ()

let () =
    Aoc.time_fn part1;
    Aoc.time_fn part2
