open Core

type machine = {
    a : int * int;
    b : int * int;
    prize : int * int;
  }
[@@deriving show]

let button =
    let open Re in
    let move = group @@ seq [ set "+-"; rep digit ] in
    seq [ rep any; str ": "; str "X"; move; rep any; move ] |> compile

let prize =
    let open Re in
    seq [ rep any; str ": "; str "X="; group @@ rep digit; str ", Y="; group @@ rep digit ]
    |> compile

let parse_machine part2 l =
    let offset = if part2 then 10000000000000 else 0 in
    match l with
    | [ a; b; p ] ->
        let r = Re.exec button a in
        let ax = Re.Group.get r 1 |> Int.of_string in
        let ay = Re.Group.get r 2 |> Int.of_string in

        let r = Re.exec button b in
        let bx = Re.Group.get r 1 |> Int.of_string in
        let by = Re.Group.get r 2 |> Int.of_string in

        let r = Re.exec prize p in
        let px = (Re.Group.get r 1 |> Int.of_string) + offset in
        let py = (Re.Group.get r 2 |> Int.of_string) + offset in

        { a = (ax, ay); b = (bx, by); prize = (px, py) }
    | _ -> failwith "Invalid machine"

let solve m =
    let a1, a2 = m.a |> Tuple2.map ~f:Int.to_float in
    let b1, b2 = m.b |> Tuple2.map ~f:Int.to_float in
    let c1, c2 = m.prize |> Tuple2.map ~f:Int.to_float in
    let x = ((c1 *. b2) -. (b1 *. c2)) /. ((a1 *. b2) -. (b1 *. a2)) in
    let y = ((a1 *. c2) -. (c1 *. a2)) /. ((a1 *. b2) -. (b1 *. a2)) in
    if Float.is_integer x && Float.is_integer y then
      Some ((x, y) |> Tuple2.map ~f:Int.of_float)
    else
      None

let () =
    let input = Aoc.read_to_iter "day13" |> Aoc.split_iter ~on:String.is_empty in
    let part1 = Iter.map (parse_machine false) input in
    let part2 = Iter.map (parse_machine true) input in
    let s1, s2 =
        (part1, part2)
        |> Tuple2.map ~f:(fun m ->
               m
               |> Iter.map solve
               |> Iter.fold
                    (fun acc v ->
                      match v with
                      | None -> acc
                      | Some (a, b) -> acc + ((a * 3) + b))
                    0)
    in
    Printf.printf "Part 1: %d\n" s1;
    Printf.printf "Part 2: %d\n" s2
