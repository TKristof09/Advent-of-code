open Core

let calc_next secret =
    let mix old new_ = old lxor new_ in
    let prune num = num mod 16777216 in
    let secret = secret * 64 |> mix secret |> prune in
    let secret = secret / 32 |> mix secret |> prune in
    secret * 2048 |> mix secret |> prune

let calc n secret = Sequence.range 0 n |> Sequence.fold ~init:secret ~f:(fun acc _ -> calc_next acc)

module Tuple4 = struct
  module T = struct
    type t = int * int * int * int [@@deriving compare, sexp, hash, show]
  end

  include T
  include Comparable.Make_plain (T)
  include Hashable.Make_plain (T)
end

let calc_diff n secret =
    let a = calc_next secret in
    let b = calc_next a in
    let c = calc_next b in
    let d = calc_next c in
    let asec = (a mod 10) - (secret mod 10) in
    let ba = (b mod 10) - (a mod 10) in
    let cb = (c mod 10) - (b mod 10) in
    let dc = (d mod 10) - (c mod 10) in
    let diffs = (asec, ba, cb, dc) in
    let tbl = Hashtbl.create (module Tuple4) in
    Hashtbl.set tbl ~key:diffs ~data:(d mod 10);
    let _ =
        Sequence.range 4 n
        |> Sequence.fold
             ~init:(d, (diffs, d mod 10))
             ~f:(fun (secret, l) x ->
               let (d1, d2, d3, d4), p = l in
               let s = calc_next secret in
               let newp = s mod 10 in
               let d = newp - p in
               Hashtbl.update tbl (d2, d3, d4, d) ~f:(fun v ->
                   match v with
                   | None -> newp
                   | Some x -> x);
               (s, ((d2, d3, d4, d), newp)))
    in
    tbl

let () =
    let input = Aoc.read_to_array "day22" |> Array.map ~f:Int.of_string in
    Array.sum (module Int) input ~f:(calc 2000) |> Printf.printf "Part 1: %d\n";
    let tbl = Hashtbl.create (module Tuple4) in
    Array.iter input ~f:(fun n ->
        let res = calc_diff 2000 n in
        Hashtbl.merge_into ~src:res ~dst:tbl ~f:(fun ~key a b ->
            match b with
            | None -> Hashtbl_intf.Merge_into_action.Set_to a
            | Some x -> Hashtbl_intf.Merge_into_action.Set_to (a + x)));
    Hashtbl.fold tbl ~init:0 ~f:(fun ~key ~data acc -> max acc data) |> Printf.printf "Part 2: %d\n"
