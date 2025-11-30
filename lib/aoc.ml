open Core

let read_to_list filename = In_channel.read_lines ("inputs/" ^ filename)

let read_to_list_filtered filename =
    In_channel.read_lines ("inputs/" ^ filename)
    |> List.filter ~f:(fun s -> not (String.is_empty s))

let read_to_array filename = Array.of_list @@ read_to_list filename
let read_to_array_filtered filename = Array.of_list @@ read_to_list_filtered filename
let read_to_iter filename = read_to_list filename |> Iter.of_list

let read_to_iter_filetered filename =
    read_to_iter filename |> Iter.filter (fun s -> not (String.is_empty s))

let split_list l ~on =
    let open IterLabels in
    of_list l
    |> group_succ_by ~eq:(fun x y -> (not (on x)) && not (on y))
    |> filter ~f:(function
         | [] -> false
         | h :: _ -> not (on h))
    |> map ~f:List.rev

let split_iter iter ~on =
    iter
    |> IterLabels.group_succ_by ~eq:(fun x y -> (not (on x)) && not (on y))
    |> IterLabels.filter ~f:(function
         | [] -> false
         | h :: _ -> not (on h))
    |> IterLabels.map ~f:List.rev

module Pair = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp, hash, show]
  end

  include T
  include Comparable.Make_plain (T)
  include Hashable.Make_plain (T)
end

let read_to_map filename filter =
    let arr = read_to_array filename in
    Array.foldi arr
      ~init:(Map.empty (module Pair))
      ~f:(fun y m line ->
        String.foldi line ~init:m ~f:(fun x m' c ->
            if filter c then Map.set m' ~key:(x, y) ~data:c else m'))

let string_to_map lines filter =
    List.foldi lines
      ~init:(Map.empty (module Pair))
      ~f:(fun y m line ->
        String.foldi line ~init:m ~f:(fun x m' c ->
            if filter c then Map.set m' ~key:(x, y) ~data:c else m'))

let show_coord_map m = Printf.sprintf "%s" ([%derive.show: (Pair.t * char) list] (Map.to_alist m))

let show_binary =
    let int_size = 63 in
    let open Stdlib in
    let buf = Bytes.create int_size in
    fun n ->
      for i = 0 to int_size - 1 do
        let pos = int_size - 1 - i in
        Bytes.set buf pos (if n land (1 lsl i) != 0 then '1' else '0')
      done;
      (* skip leading zeros *)
      match Bytes.index_opt buf '1' with
      | None -> "0b0"
      | Some i -> "0b" ^ Bytes.sub_string buf i (int_size - i)
