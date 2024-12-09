open Core

type eq = {
    res : int;
    operands : int list;
  }
[@@deriving show]

type op =
    | Add
    | Mul
    | Concat
[@@deriving show]

let parse_input input =
    List.map input ~f:(fun line ->
        let res, operands = String.lsplit2_exn line ~on:':' in
        let operands = String.strip operands |> String.split ~on:' ' |> List.map ~f:Int.of_string in
        { res = Int.of_string res; operands })

let eval op x y =
    match op with
    | Add -> x + y
    | Mul -> x * y
    | Concat ->
        let x = Int.to_string x in
        let y = Int.to_string y in
        String.concat [ x; y ] |> Int.of_string

let rec permutations n ops =
    match n with
    | 0 -> Sequence.singleton []
    | _ ->
        Sequence.concat_map (Sequence.of_list ops) ~f:(fun op ->
            Sequence.map
              (permutations (n - 1) ops)
              ~f:(fun sub_permutation -> op :: sub_permutation))

let all_op_permutations n = permutations n [ Add; Mul; Concat ]

let is_possible eq =
    let ops_perms = all_op_permutations (List.length eq.operands - 1) in
    let check_ops ops =
        List.fold2_exn ops (List.tl_exn eq.operands) ~init:(List.hd_exn eq.operands)
          ~f:(fun res op x ->
            if res = -1 then
              -1
            else
              let res = eval op res x in
              if res > eq.res then -1 else res)
    in
    Sequence.fold_until ops_perms ~init:false
      ~f:(fun _ ops ->
        let res = check_ops ops in
        if res = eq.res then
          Stop true
        else
          Continue false)
      ~finish:(fun _ -> false)

let () =
    Aoc.read_to_list_filtered "day7"
    |> parse_input
    |> List.fold ~init:0 ~f:(fun acc eq -> if is_possible eq then acc + eq.res else acc)
    |> Printf.printf "%d\n"
