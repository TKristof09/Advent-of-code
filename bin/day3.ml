open Core

let find_max s num_digits =
    let rec aux chop_end str =
        if chop_end >= 0 then
          let digit, digit_idx =
              String.drop_suffix str chop_end
              |> String.foldi ~init:(0, 0) ~f:(fun idx (m, best_idx) c ->
                  let i = Char.get_digit_exn c in
                  if i > m then (i, idx) else (m, best_idx))
          in
          digit :: aux (chop_end - 1) (String.drop_prefix str (digit_idx + 1))
        else
          []
    in
    aux (num_digits - 1) s
    |> List.foldi ~init:0 ~f:(fun idx res d -> res + (d * Int.pow 10 (num_digits - idx - 1)))

let part1 () = Aoc.read_to_list "day3" |> List.sum (module Int) ~f:(fun s -> find_max s 2)
let part2 () = Aoc.read_to_list "day3" |> List.sum (module Int) ~f:(fun s -> find_max s 12)

(* let () = *)
(*     Aoc.read_to_list "day3" *)
(*     |> List.map ~f:(fun s -> find_max s 12) *)
(*     |> [%derive.show: int list] *)
(*     |> Printf.printf "%s\n" *)
let () =
    part1 () |> Printf.printf "Part 1: %d\n";
    part2 () |> Printf.printf "Part 2: %d\n"

let () =
    Printf.printf "Part 1: ";
    Aoc.time_fn part1;
    Printf.printf "Part 2: ";
    Aoc.time_fn part2
