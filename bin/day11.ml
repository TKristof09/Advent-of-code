open Core

let rec blink l =
    match l with
    | [] -> []
    | h :: t ->
        if h = 0 then
          1 :: blink t
        else
          let s = Int.to_string h in
          let l = String.length s in
          if l mod 2 = 0 then
            let l, r = (String.sub s ~pos:0 ~len:(l / 2), String.sub s ~pos:(l / 2) ~len:(l / 2)) in
            Int.of_string l :: Int.of_string r :: blink t
          else
            (h * 2024) :: blink t

let tbl = Hashtbl.create (module Int)

let rec dp i l =
    let update_tbl h res =
        if i > 10 then
          Hashtbl.update tbl h ~f:(function
            | None ->
                let tbl' = Hashtbl.create (module Int) in
                Hashtbl.set tbl' ~key:i ~data:res;
                tbl'
            | Some tbl' ->
                Hashtbl.set tbl' ~key:i ~data:res;
                tbl')
    in
    if i = 0 then
      List.length l
    else
      match l with
      | [] -> 0
      | h :: t -> (
          match Hashtbl.find tbl h with
          | None ->
              let res = dp (i - 1) (blink [ h ]) in
              update_tbl h res;
              res + dp i t
          | Some tbl' -> (
              match Hashtbl.find tbl' i with
              | None ->
                  let res = dp (i - 1) (blink [ h ]) in
                  update_tbl h res;
                  res + dp i t
              | Some res -> res + dp i t))

let part1 =
    Aoc.read_to_list "day11"
    |> List.hd_exn
    |> String.split ~on:' '
    |> List.map ~f:Int.of_string
    |> Iter.iterate blink
    |> Iter.drop 25
    |> Iter.head_exn
    |> List.length
    |> Printf.printf "Part 1: %d\n"

let part2 =
    Aoc.read_to_list "day11"
    |> List.hd_exn
    |> String.split ~on:' '
    |> List.map ~f:Int.of_string
    |> dp 75
    |> Printf.printf "Part 2: %d\n"

let () =
    part1;
    part2
