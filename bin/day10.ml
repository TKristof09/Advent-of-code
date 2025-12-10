open Core

type button = int

type machine = {
    lights : int;
    buttons : button list;
    joltage : int list;
  }

let pp_button fmt button =
    let set_bit_indices n =
        let rec aux idx num acc =
            if num = 0 then
              List.rev acc
            else if num mod 2 = 1 then
              aux (idx + 1) (num / 2) (idx :: acc)
            else
              aux (idx + 1) (num / 2) acc
        in
        aux 0 n []
    in

    let indices = set_bit_indices button in
    Format.fprintf fmt "(%a)"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",") Format.pp_print_int)
      indices

let pp_machine fmt (m : machine) =
    let pp_buttons fmt buttons =
        Format.fprintf fmt "[%a]"
          (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ") pp_button)
          buttons
    in

    let pp_joltage fmt joltage =
        Format.fprintf fmt "[%a]"
          (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ") Format.pp_print_int)
          joltage
    in

    Format.fprintf fmt "{ lights: %d; buttons: %a; joltage: %a }" m.lights pp_buttons m.buttons
      pp_joltage m.joltage

let regex = Pcre.regexp "^\\[([\\.#]+)\\] ((?:\\((?:\\d+,?)+\\) )+){((?:\\d+,?)+)}$"
let regex_buttons = Pcre.regexp "\\(((?:\\d+,?)+)\\)"

let parse s =
    let matches = Pcre.extract ~full_match:false ~rex:regex s in
    let lights =
        String.rev matches.(0)
        |> String.fold ~init:0 ~f:(fun acc c ->
            if Char.equal c '.' then
              acc lsl 1
            else
              (acc lsl 1) lor 1)
    in
    let joltage = String.split matches.(2) ~on:',' |> List.map ~f:Int.of_string in
    let matches_buttons = Pcre.extract_all ~full_match:false ~rex:regex_buttons matches.(1) in
    let buttons =
        matches_buttons
        |> Array.map ~f:(fun arr ->
            String.split arr.(0) ~on:','
            |> List.map ~f:Int.of_string
            |> List.fold ~init:0 ~f:(fun acc i ->
                assert (i <= 63);
                acc lor (1 lsl i)))
        |> Array.to_list
    in
    { lights; buttons; joltage }

let solve_lp m =
    let num_buttons = List.length m.buttons in
    let presses = Lp.range ~integer:true ~lb:0.0 num_buttons "presses" in
    let num_presses = Lp.concat presses in
    let obj = Lp.minimize num_presses in
    let constraints =
        List.mapi m.joltage ~f:(fun i jolt ->
            let open Lp in
            let lhs =
                List.mapi m.buttons ~f:(fun j b ->
                    let coeff = (b lsr i) land 1 in
                    c (float_of_int coeff) *~ presses.(j))
                |> List.fold ~init:zero ~f:( ++ )
            in
            Cnstr.eq lhs (c (float_of_int jolt)))
    in
    let prob = Lp.make obj constraints in
    (* Lp.write "day10.lp" prob; *)
    match Lp_glpk.solve ~term_output:false prob with
    | Ok (_, xs) -> Array.sum (module Int) presses ~f:(fun p -> Lp.PMap.find p xs |> int_of_float)
    | Error msg -> failwith msg

let solve machine =
    let initial_state = 0 in
    let heap = Pairing_heap.create ~cmp:(fun (_, d, _) (_, d', _) -> Int.compare d d') () in
    Pairing_heap.add heap (initial_state, 0, []);
    let visited = Hash_set.create ~size:1000 (module Int) in
    let rec search best =
        let _, best_d, _ = best in
        match Pairing_heap.pop heap with
        | None -> best
        | Some (s, d, path) ->
            if d >= best_d then
              search best
            else if s = machine.lights then
              search (s, d, path)
            else if Hash_set.mem visited s then
              search best
            else (
              Hash_set.add visited s;
              List.iter machine.buttons ~f:(fun b ->
                  Pairing_heap.add heap (s lxor b, d + 1, b :: path));
              search best)
    in
    search (initial_state, Int.max_value, [])

(* module IntList = struct *)
(*   module T = struct *)
(*     type t = int list [@@deriving compare, sexp, hash, show] *)
(*   end *)
(**)
(*   include T *)
(*   include Comparable.Make_plain (T) *)
(*   include Hashable.Make_plain (T) *)
(* end *)
(* let filter_moves buttons cur_jolts goal = *)
(*     let remaining = *)
(*         String.mapi cur_jolts ~f:(fun i c -> Char.(to_int goal.[i] - to_int c) |> Char.of_int_exn) *)
(*     in *)
(*     let min_i, _ = *)
(*         String.foldi remaining ~init:(0, Char.max_value) ~f:(fun i (min_idx, min_c) c -> *)
(*             if Char.(c < min_c && c <> '\000') then (i, c) else (min_idx, min_c)) *)
(*     in *)
(*     let buttons = *)
(*         List.filter buttons ~f:(fun b -> *)
(*             b land (1 lsl min_i) <> 0 *)
(*             && String.for_alli remaining ~f:(fun i c -> *)
(*                 if b land (1 lsl i) <> 0 then Char.to_int c <> 0 else true)) *)
(*     in *)
(*     buttons *)
(**)
(* let solve2 machine = *)
(*     let heap = Pairing_heap.create ~cmp:(fun (_, d) (_, d') -> Int.compare d d') () in *)
(*     let goal = String.of_list (List.map machine.joltage ~f:Char.of_int_exn) in *)
(*     let initial_state = *)
(*         String.init (List.length machine.joltage) ~f:(Fun.const (Char.of_int_exn 0)) *)
(*     in *)
(*     Pairing_heap.add heap (initial_state, 0); *)
(*     let visited = Hash_set.create ~size:1000 (module String) in *)
(*     let rec search best idx = *)
(*         if idx mod 1000000 = 0 then Gc.full_major (); *)
(*         let _, best_d = best in *)
(*         match Pairing_heap.pop heap with *)
(*         | None -> best *)
(*         | Some (s, d) -> *)
(*             if d >= best_d then *)
(*               search best idx *)
(*             else if String.equal goal s then *)
(*               search (s, d) idx *)
(*             else ( *)
(*               Hash_set.add visited s; *)
(*               let moves = filter_moves machine.buttons s goal in *)
(*               (* Printf.printf "%d: %s -> %s   { %s }\n" d *) *)
(*               (*   ([%derive.show: int list] (String.to_list s |> List.map ~f:Char.to_int)) *) *)
(*               (*   ([%derive.show: button list] moves) *) *)
(*               (*   ([%derive.show: button list] path); *) *)
(*               moves *)
(*               |> List.iter ~f:(fun b -> *)
(*                   let new_joltages = *)
(*                       String.mapi s ~f:(fun i jolts -> *)
(*                           Char.to_int jolts + ((b lsr i) land 1) |> Char.of_int_exn) *)
(*                   in *)
(*                   Pairing_heap.add heap (new_joltages, d + 1)); *)
(*               search best (idx + 1)) *)
(*     in *)
(*     search (initial_state, Int.max_value) 1 *)

let inp = Aoc.read_to_list "day10"
(* let () = inp |> List.map ~f:parse |> [%derive.show: machine list] |> Printf.printf "%s\n" *)

let part1 () = inp |> List.map ~f:parse |> List.map ~f:solve |> List.sum (module Int) ~f:Tuple3.get2
(* |> [%derive.show: (int * int * button list) list] *)

(* let part2_bad () = *)
(*     inp *)
(*     |> List.map ~f:parse *)
(*     |> List.mapi ~f:(fun i m -> *)
(*         let res = solve2 m in *)
(*         Printf.printf "%d -- %d\n" i (snd res); *)
(*         Out_channel.flush Out_channel.stdout; *)
(*         Gc.compact (); *)
(*         Gc.full_major (); *)
(*         res) *)
(*     |> List.sum (module Int) ~f:Tuple2.get2 *)
(*     (* |> [%derive.show: (int * int * button list) list] *) *)
(*     |> Printf.printf "Part 2: %d\n" *)

let part2 () = inp |> List.map ~f:parse |> List.sum (module Int) ~f:solve_lp

let () =
    part1 () |> Printf.printf "Part 1: %d\n";
    part2 () |> Printf.printf "Part 2: %d\n"

let () =
    Aoc.time_fn part1;
    Aoc.time_fn part2
