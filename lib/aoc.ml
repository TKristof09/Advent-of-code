open Core

let read_to_list filename = In_channel.read_lines ("inputs/" ^ filename)

let read_to_list_filtered filename =
    In_channel.read_lines ("inputs/" ^ filename)
    |> List.filter ~f:(fun s -> not (String.is_empty s))

let read_to_array filename = Array.of_list @@ read_to_list filename
let read_to_array_filtered filename = Array.of_list @@ read_to_list_filtered filename

module Pair = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp, show]
  end

  include T
  include Comparable.Make_plain (T)
end

let read_to_map filename filter =
    let arr = read_to_array filename in
    Array.foldi arr
      ~init:(Map.empty (module Pair))
      ~f:(fun y m line ->
        String.foldi line ~init:m ~f:(fun x m' c ->
            if filter c then Map.set m' ~key:(x, y) ~data:c else m'))

let show_coord_map m = Printf.sprintf "%s" ([%derive.show: (Pair.t * char) list] (Map.to_alist m))
