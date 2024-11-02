open Core

module Pair = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp]
  end
  include T
  include Comparable.Make_plain(T)
end

let parse_line line_num text = 
    let is_part_number x y = 
        let is_symbol ch = 
            let open Core.Char in (* otherwise the <> opertator gets weird because it is shadowed by the <> on ints *)
            (not (Char.is_digit ch)) && (ch <> '.')
        in
        let height = Array.length text in
        let width = String.length text.(y) in

        let is_valid_position x y = 
            x >= 0 && x < width && y >= 0 && y < height 
        in

        let neighbors = [
            (y - 1, x - 1); (y - 1, x); (y - 1, x + 1);  (* Top-left, Top, Top-right *)
            (y, x - 1);                 (y, x + 1);        (* Left, Right *)
            (y + 1, x - 1); (y + 1, x); (y + 1, x + 1);  (* Bottom-left, Bottom, Bottom-right *)
        ] in

        List.exists neighbors ~f:(fun (ny, nx) ->
            is_valid_position nx ny && is_symbol text.(ny).[nx]
        ) 
    in

    let nums, last = String.foldi text.(line_num) ~init: ([], []) ~f:(fun i (acc, cur) ch ->
            if Char.is_digit ch then
                (acc, (Char.get_digit_exn ch, is_part_number i line_num) :: cur)
            else
                match List.exists cur ~f:(fun (_, good) -> good) with
                | true -> ((List.foldi cur ~init:0 ~f:(fun i acc (n, _) -> acc + n * (Int.pow 10 i)))::acc, [])
                | false -> (acc, [])
        )
    in 
    let nums = match List.exists last ~f:(fun (_, good) -> good) with
        | true -> (List.foldi last ~init:0 ~f:(fun i acc (n, _) -> acc + n * (Int.pow 10 i)))::nums
        | false -> nums
    in
    List.fold nums ~init:0 ~f:(fun acc n -> Printf.printf "%d: %d\n" line_num n; acc + n)



let find_gear_ratio line_num text gear_map = 
    let find_gear x y = 
        let is_symbol ch = 
            let open Core.Char in (* otherwise the <> opertator gets weird because it is shadowed by the <> on ints *)
            ch = '*'
        in
        let height = Array.length text in
        let width = String.length text.(y) in

        let is_valid_position x y = 
            x >= 0 && x < width && y >= 0 && y < height 
        in

        let neighbors = [
            (y - 1, x - 1); (y - 1, x); (y - 1, x + 1);  (* Top-left, Top, Top-right *)
            (y, x - 1);                 (y, x + 1);        (* Left, Right *)
            (y + 1, x - 1); (y + 1, x); (y + 1, x + 1);  (* Bottom-left, Bottom, Bottom-right *)
        ] in

        List.filter neighbors ~f:(fun (ny, nx) ->
            is_valid_position nx ny && is_symbol text.(ny).[nx]
        ) 
    in
    (* cur = digits, gear_location_list_list
       acc = gear_map
    *)
    let gear_map, _ = String.foldi text.(line_num) ~init: (gear_map, ([], [])) ~f:(fun i (acc, cur) ch ->
            if Char.is_digit ch then
                let num, gears = cur in
                (acc, ((Char.get_digit_exn ch) :: num, (find_gear i line_num) :: gears))
            else
                match cur with
                | _, [] -> (acc, ([], []))
                | num, gears -> 
                        let cur_num = List.foldi num ~init:0 ~f:(fun i acc n -> acc + n * (Int.pow 10 i)) in
                        (* add gears to map (x,y) -> list of numbers *)
                        let gear_map = List.fold gears ~init:acc ~f:(fun acc' gears' -> 
                            List.fold gears' ~init:acc' ~f:(fun acc'' gear -> 
                                Map.update acc''  gear ~f:(fun elt ->
                                    match elt with
                                        | None -> Map.set (Map.empty (module Pair)) ~key:(line_num, i) ~data:cur_num
                                        | Some nums -> Map.set nums ~key:(line_num, i) ~data: cur_num
                                )
                        ))
                        in (gear_map, ([], []))
        )
    in 
    gear_map

let calc_ratio gear_map =
    Map.fold gear_map ~init:0 ~f:(fun ~key ~data acc->
        let x,y = key in
        Printf.printf "]\nACC: %d ----- (%d,%d) -> [" acc x y;
        match Map.length data with
            | 2 -> acc + (Map.fold data ~init:1 ~f:(fun ~key:_ ~data acc -> Printf.printf "%d, " data; acc * data))
            | _ -> acc
    )

let () =
    let lines = Aoc.read_to_array "day3" in
    let gear_map = Map.empty (module Pair) in 
    Array.foldi lines ~init:gear_map ~f:(fun i gear_map _ -> (find_gear_ratio i lines gear_map)) 
    |> calc_ratio
    |> Printf.printf "\n%d\n" 


