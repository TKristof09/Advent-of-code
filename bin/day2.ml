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

let gen_range s =
    let start, stop = String.lsplit2_exn ~on:'-' s |> Tuple2.map ~f:Int.of_string in
    Iter.int_range ~start ~stop

let parse_range s = String.lsplit2_exn ~on:'-' s

(** Split range into multiple ranges where each start and end have same number of digits *)
let rec split_range (start, stop) =
    let l_start = String.length start in
    let l_stop = String.length stop in
    if l_start < l_stop then
      let r_stop = Int.pow 10 l_start - 1 in
      (start, Int.to_string r_stop) :: split_range (Int.to_string (r_stop + 1), stop)
    else
      [ (start, stop) ]

let get_repeating (start, stop) ~len_rep ~len_full =
    let start_rep = String.prefix start len_rep |> Int.of_string in
    let end_rep = String.prefix stop len_rep |> Int.of_string in
    let start_n = Int.of_string start
    and end_n = Int.of_string stop in
    let gen_number rep =
        String.concat (List.init (len_full / len_rep) ~f:(fun _ -> rep)) |> Int.of_string
    in
    Iter.int_range ~start:start_rep ~stop:end_rep
    |> IterLabels.map ~f:(fun i -> Int.to_string i |> gen_number)
    |> IterLabels.filter ~f:(fun n -> start_n <= n && n <= end_n)

let solve1 () =
    Aoc.read_to_list "day2"
    |> List.hd_exn
    |> String.split ~on:','
    |> List.map ~f:gen_range
    |> List.map ~f:(fun range ->
        IterLabels.fold range ~init:0 ~f:(fun res i ->
            let s = Int.to_string i in
            if is_repeating s then
              res + i
            else
              res))
    |> List.sum (module Int) ~f:Fun.id
    |> Printf.printf "Part 1 Sum: %d\n"

let solve1_fast () =
    Aoc.read_to_list "day2"
    |> List.hd_exn
    |> String.split ~on:','
    |> Iter.of_list
    |> IterLabels.map ~f:parse_range
    |> IterLabels.flat_map_l ~f:split_range
    |> IterLabels.filter ~f:(fun (start, stop) -> String.length start mod 2 = 0)
    |> IterLabels.flat_map ~f:(fun (start, stop) ->
        let l = String.length start in
        get_repeating (start, stop) ~len_rep:(l / 2) ~len_full:l)
    |> IterLabels.fold ~init:Int.Set.empty ~f:Set.add
    |> Set.sum (module Int) ~f:Fun.id
    |> Printf.printf "Part 1 Sum: %d   (fast)\n"

let solve2 () =
    Aoc.read_to_list "day2"
    |> List.hd_exn
    |> String.split ~on:','
    |> Iter.of_list
    |> IterLabels.map ~f:gen_range
    |> IterLabels.map ~f:(fun range ->
        IterLabels.fold range ~init:0 ~f:(fun res i ->
            let s = Int.to_string i in
            if is_repeating2 s then
              res + i
            else
              res))
    |> Iter.sum
    |> Printf.printf "Part 2 Sum: %d\n"

let solve2_bis () =
    Aoc.read_to_list "day2"
    |> List.hd_exn
    |> String.split ~on:','
    |> Iter.of_list
    |> IterLabels.map ~f:gen_range
    |> IterLabels.map ~f:(fun range ->
        IterLabels.fold range ~init:0 ~f:(fun res i ->
            let s = Int.to_string i in
            if is_repeating2_noregex s then
              res + i
            else
              res))
    |> Iter.sum
    |> Printf.printf "Part 2 Sum: %d   (no regex)\n"

let solve2_fast () =
    Aoc.read_to_list "day2"
    |> List.hd_exn
    |> String.split ~on:','
    |> Iter.of_list
    |> IterLabels.map ~f:parse_range
    |> IterLabels.flat_map_l ~f:split_range
    |> IterLabels.filter ~f:(fun (start, stop) -> String.length start > 1)
    |> IterLabels.flat_map ~f:(fun (start, stop) ->
        Iter.int_range ~start:1 ~stop:((String.length start + 1) / 2)
        |> IterLabels.flat_map ~f:(fun l ->
            get_repeating (start, stop) ~len_rep:l ~len_full:(String.length start)))
    |> IterLabels.fold ~init:Int.Set.empty ~f:Set.add
    |> Set.sum (module Int) ~f:Fun.id
    |> Printf.printf "Part 2 Sum: %d   (fast)\n"

let () =
    Aoc.time_fn solve1;
    Aoc.time_fn solve1_fast;
    Aoc.time_fn solve2;
    Aoc.time_fn solve2_bis;
    Aoc.time_fn solve2_fast
