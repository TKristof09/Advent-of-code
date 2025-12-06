open Core

type op =
    | Plus
    | Times
[@@deriving show { with_path = false }]

let parse_op s =
    match s with
    | "+" -> Plus
    | "*" -> Times
    | _ -> assert false

let parse_input =
    Array.fold ~init:([], []) ~f:(fun (nums, _) s ->
        let l = String.split ~on:' ' s |> List.filter ~f:(fun s -> not (String.is_empty s)) in
        match l with
        | h :: t when Int.of_string_opt h |> Option.is_some ->
            (List.map (h :: t) ~f:Int.of_string :: nums, [])
        | h :: t -> (nums, List.map (h :: t) ~f:parse_op)
        | _ -> failwith s)

let parse_input2 arr =
    let ops =
        Array.last arr
        |> String.fold ~init:[] ~f:(fun ops c ->
            match c with
            | '+' -> Plus :: ops
            | '*' -> Times :: ops
            | ' ' -> ops
            | _ -> assert false)
    in
    let nums =
        Array.map (Array.sub arr ~pos:0 ~len:(Array.length arr - 1)) ~f:String.to_array
        |> Array.transpose_exn
        |> Array.map ~f:(fun a -> String.of_array a |> String.rev)
        |> Array.to_list
        |> List.group ~break:(fun s1 s2 ->
            let p1 = String.exists ~f:(fun c -> not (Char.is_whitespace c)) s1 in
            let p2 = String.exists ~f:(fun c -> not (Char.is_whitespace c)) s2 in
            not (p1 && p2))
        |> List.filter_map ~f:(fun l ->
            if List.hd_exn l |> String.for_all ~f:Char.is_whitespace then
              None
            else
              Some (List.map l ~f:(fun s -> s |> String.rev |> String.strip |> Int.of_string)))
    in
    (nums, List.rev ops)

let rec fold3 l1 l2 l3 ~init ~f =
    match (l1, l2, l3) with
    | [], [], [] -> init
    | a1 :: l1, a2 :: l2, a3 :: l3 -> fold3 l1 l2 l3 ~f ~init:(f init a1 a2 a3)
    | _, _, _ -> assert false

let solve nums ops =
    List.fold nums
      ~init:
        (List.map ops ~f:(fun op ->
             match op with
             | Plus -> 0
             | Times -> 1))
      ~f:(fun res operands ->
        fold3 res operands ops ~init:[] ~f:(fun res r operand op ->
            match op with
            | Plus -> (r + operand) :: res
            | Times -> (r * operand) :: res)
        |> List.rev)

let solve2 nums ops =
    List.fold2_exn nums ops ~init:0 ~f:(fun res nums op ->
        let init =
            match op with
            | Plus -> 0
            | Times -> 1
        in
        let intermediate =
            List.fold nums ~init ~f:(fun r n ->
                match op with
                | Plus -> n + r
                | Times -> n * r)
        in
        res + intermediate)

let inp = Aoc.read_to_array "day6"

let part1 () =
    parse_input inp
    |> Tuple2.uncurry solve
    |> List.sum (module Int) ~f:Fun.id
    |> Printf.printf "Part 1: %d\n"

let part2 () = parse_input2 inp |> Tuple2.uncurry solve2 |> Printf.printf "Part 2: %d\n"

let () =
    Aoc.time_fn part1;
    Aoc.time_fn part2
