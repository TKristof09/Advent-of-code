open Core

let parse input =
    let tbl = Hashtbl.create (module String) in
    List.iter input ~f:(fun s ->
        let l, r = String.lsplit2_exn s ~on:'-' in
        Hashtbl.update tbl l ~f:(function
          | None -> [ r ]
          | Some x -> r :: x);
        Hashtbl.update tbl r ~f:(function
          | None -> [ l ]
          | Some x -> l :: x));
    tbl

let rec choose k l =
    if k = 0 then
      [ [] ]
    else
      let len = List.length l in
      if len < k then
        []
      else if k = len then
        [ l ]
      else
        match l with
        | h :: t ->
            let starting_with_h = List.map (choose (k - 1) t) ~f:(fun sublist -> h :: sublist) in
            let not_starting_with_h = choose k t in
            starting_with_h @ not_starting_with_h
        | [] -> assert false

let rec power_set lst =
    match lst with
    | [] -> [ [] ]
    | head :: tail ->
        let rest_subsets = power_set tail in
        rest_subsets @ List.map ~f:(fun subset -> head :: subset) rest_subsets

module StringList = struct
  module T = struct
    type t = string list [@@deriving hash, compare, sexp]
  end

  include T
  include Comparable.Make_plain (T)
  include Hashable.Make_plain (T)
end

let is_clique tbl l =
    List.for_all l ~f:(fun x ->
        let neighbours = Set.of_list (module String) (x :: Hashtbl.find_exn tbl x) in
        List.for_all l ~f:(Set.mem neighbours))

let max_clique tbl =
    Hashtbl.fold tbl ~init:([], 0) ~f:(fun ~key ~data ((cur_max, len) as acc) ->
        if List.length data < len - 1 then
          (cur_max, len)
        else
          let cliques =
              power_set data
              |> List.filter_map ~f:(fun l ->
                     let x = key :: l in
                     let n = List.length x in
                     if n > len && is_clique tbl x then
                       Some (x, n)
                     else
                       None)
          in
          List.max_elt (acc :: cliques) ~compare:(fun (_, a) (_, b) -> Int.compare a b)
          |> Option.value_exn)
    |> Tuple2.get1

let solve1 tbl =
    Hashtbl.fold tbl
      ~init:(Set.empty (module StringList))
      ~f:(fun ~key ~data acc ->
        let n = List.length data in
        if String.is_prefix key ~prefix:"t" && n >= 2 then
          List.fold (choose 2 data) ~init:acc ~f:(fun acc x ->
              if is_clique tbl (key :: x) then
                Set.add acc (List.sort (key :: x) ~compare:String.compare)
              else
                acc)
        else
          acc)
    |> Set.length

let () =
    let graph = Aoc.read_to_list "day23" |> parse in
    graph |> solve1 |> Printf.printf "Part 1: %d\n";
    graph
    |> max_clique
    |> List.sort ~compare:String.compare
    |> String.concat ~sep:","
    |> Printf.printf "Part 2: %s\n"
