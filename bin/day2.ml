open Core

let is_repeating s =
    let l = String.length s in
    l mod 2 = 0 && String.equal (String.prefix s (l / 2)) (String.suffix s (l / 2))

let regex = Pcre.regexp "^(\\d+)\\1+$"
let is_repeating2 s = Pcre.pmatch ~rex:regex s

let split_string_eql_length s ~len =
    let l = String.length s in
    assert (l mod len = 0);
    String.to_list s
    |> List.groupi ~break:(fun i _ _ -> i mod len = 0)
    |> List.map ~f:String.of_char_list

let is_repeating2_noregex s =
    let l = String.length s in
    Iter.int_range ~start:1 ~stop:(l / 2)
    |> IterLabels.filter ~f:(fun i -> l mod i = 0)
    |> IterLabels.filter_map ~f:(fun i ->
        let substrings = split_string_eql_length s ~len:i in
        List.all_equal substrings ~equal:String.equal |> Option.map ~f:Int.of_string)
    |> IterLabels.is_empty
    |> not

let parse_range s =
    let start, stop = String.lsplit2_exn ~on:'-' s |> Tuple2.map ~f:Int.of_string in
    Iter.int_range ~start ~stop

let () =
    Aoc.read_to_list "day2"
    |> List.hd_exn
    |> String.split ~on:','
    |> List.map ~f:parse_range
    |> List.map ~f:(fun range ->
        IterLabels.fold range ~init:0 ~f:(fun res i ->
            let s = Int.to_string i in
            if is_repeating s then
              res + i
            else
              res))
    |> List.fold ~init:0 ~f:( + )
    |> Printf.printf "Part 1 Sum: %d\n"

let () =
    Aoc.read_to_list "day2"
    |> List.hd_exn
    |> String.split ~on:','
    |> List.map ~f:parse_range
    |> List.map ~f:(fun range ->
        IterLabels.fold range ~init:0 ~f:(fun res i ->
            let s = Int.to_string i in
            if is_repeating2 s then
              res + i
            else
              res))
    |> List.fold ~init:0 ~f:( + )
    |> Printf.printf "Part 2 Sum: %d\n"

let () =
    Aoc.read_to_list "day2"
    |> List.hd_exn
    |> String.split ~on:','
    |> List.map ~f:parse_range
    |> List.map ~f:(fun range ->
        IterLabels.fold range ~init:0 ~f:(fun res i ->
            let s = Int.to_string i in
            if is_repeating2_noregex s then
              res + i
            else
              res))
    |> List.fold ~init:0 ~f:( + )
    |> Printf.printf "Part 2 Sum: %d   (no regex)\n"
