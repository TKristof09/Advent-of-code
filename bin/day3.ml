open Core

(* let r_mul = *)
(*     let open Re in *)
(*     seq [ str "mul("; group @@ rep digit; str ","; group @@ rep digit; str ")" ] |> compile *)

let r =
    let open Re in
    let mul = seq [ str "mul("; group @@ rep digit; str ","; group @@ rep digit; str ")" ] in
    alt [ str "do()"; str "don't()"; mul ] |> compile

let () =
    let input = Aoc.read_to_list "day3" |> String.concat ~sep:"" in

    let groups = Re.all r input in
    let acc, _ =
        List.fold groups ~init:(0, true) ~f:(fun (acc, enabled) group ->
            let g = Re.Group.get group 0 in
            if String.equal g "do()" then
              (acc, true)
            else if String.equal g "don't()" then
              (acc, false)
            else if not enabled then
              (acc, false)
            else
              let lhs = Re.Group.get group 1 |> Int.of_string in
              let rhs = Re.Group.get group 2 |> Int.of_string in
              Format.printf "%d * %d\n" lhs rhs;
              (acc + (lhs * rhs), true))
    in
    Printf.printf "%d\n" acc
