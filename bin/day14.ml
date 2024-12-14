open Core

type t = {
    pos : int * int;
    v : int * int;
  }
[@@deriving show]

let r =
    let open Re in
    let num = seq [ opt (str "-"); rep digit ] in
    let coord = seq [ group @@ num; str ","; group @@ num ] in
    seq [ str "p="; coord; str " v="; coord ] |> compile

let quadrant (x, y) (sx, sy) =
    let hsx = (sx / 2) + 1 in
    let hsy = (sy / 2) + 1 in
    match (x < hsx, y < hsy) with
    | true, true -> 0
    | false, true -> 1
    | true, false -> 2
    | false, false -> 3

let print_grid (sx, sy) robots i =
    let img = Image.create_grey ~max_val:255 sx sy in
    let robots = List.fold robots ~init:(Set.empty (module Aoc.Pair)) ~f:Set.add in
    let max_row = ref 0 in
    for y = 0 to sy - 1 do
      let row = ref 0 in
      for x = 0 to sx - 1 do
        if Set.mem robots (x, y) then (
          incr row;
          Image.write_grey img x y 255)
        else (
          max_row := max !row !max_row;
          row := 0;
          Image.write_grey img x y 0)
      done
    done;
    if !max_row >= 10 then
      let fn = Printf.sprintf "images/%d.png" i in
      ImageLib_unix.writefile fn img

let check robots (sx, sy) time =
    let robots =
        robots
        |> List.map ~f:(fun t ->
               let x, y = t.pos in
               let vx, vy = t.v in
               let x = (x + (time * vx)) mod sx in
               let y = (y + (time * vy)) mod sy in
               let x = if x < 0 then x + sx else x in
               let y = if y < 0 then y + sy else y in
               (x, y))
    in
    print_grid (sx, sy) robots time

let () =
    let sx, sy = (101, 103) in
    let robots =
        Aoc.read_to_iter "day14"
        |> IterLabels.map ~f:(fun s ->
               let groups = Re.exec r s in
               let px = Re.Group.get groups 1 |> Int.of_string in
               let py = Re.Group.get groups 2 |> Int.of_string in
               let vx = Re.Group.get groups 3 |> Int.of_string in
               let vy = Re.Group.get groups 4 |> Int.of_string in
               { pos = (px, py); v = (vx, vy) })
        |> Iter.to_list
    in

    for i = 0 to 10000 do
      check robots (sx, sy) i
    done
(* |> IterLabels.filter ~f:(fun (x, y) -> x <> sx / 2 && y <> sy / 2) *)
(* |> Iter.sort ~cmp:Aoc.Pair.compare *)
(* |> IterLabels.map ~f:(fun p -> quadrant p (sx, sy)) *)
(* |> IterLabels.count ~eq:Int.equal *)
(* |> IterLabels.sort ~cmp:(fun (a, _) (b, _) -> Int.compare a b) *)
(* |> IterLabels.fold ~init:1 ~f:(fun acc (_, c) -> acc * c) *)
(* |> Printf.printf "%d\n" *)

