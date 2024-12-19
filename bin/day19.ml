open Core

let parse input =
    let l1, t = (List.hd_exn input, List.drop input 2) in
    let available = String.split l1 ~on:',' |> List.map ~f:String.strip in
    (available, t)

let memo = Hashtbl.create (module String)

let is_good availables design =
    let rec aux d =
        if String.is_empty d then
          1
        else
          match Hashtbl.find memo d with
          | None ->
              let res =
                  List.fold availables ~init:0 ~f:(fun acc t ->
                      if String.is_prefix d ~prefix:t then
                        acc + aux (String.drop_prefix d (String.length t))
                      else
                        acc)
              in
              Hashtbl.set memo ~key:d ~data:res;
              res
          | Some c -> c
    in
    aux design

let () =
    let availables, designs = Aoc.read_to_list "day19" |> parse in
    (* Printf.printf "Available: %s\n" ([%derive.show: towel list] availables); *)
    (* Printf.printf "Designs: %s\n" ([%derive.show: towel] (List.nth_exn designs 2)) *)
    List.count designs ~f:(fun d -> is_good availables d > 0) |> Printf.printf "Part 1: %d\n";
    List.sum (module Int) designs ~f:(is_good availables) |> Printf.printf "Part 2: %d\n"
