open Core

let grid_regex = Pcre.regexp "^(\\d+)x(\\d+): (.*)$"

let parse l =
    let _, presents, grids =
        List.fold l ~init:([], [], []) ~f:(fun (cur_present, presents, grids) s ->
            if Pcre.pmatch ~rex:grid_regex s then
              let grid_match = Pcre.extract ~full_match:false ~rex:grid_regex s in
              let grid =
                  ( Int.of_string grid_match.(0),
                    Int.of_string grid_match.(1),
                    String.split grid_match.(2) ~on:' ' |> List.map ~f:Int.of_string )
              in
              ([], presents, grid :: grids)
            else if String.is_empty s then (
              assert (List.length cur_present = 3);
              ([], cur_present :: presents, grids))
            else if Char.equal s.[0] '.' || Char.equal s.[0] '#' then (
              assert (String.length s = 3);
              (s :: cur_present, presents, grids))
            else
              ([], presents, grids))
    in
    (List.rev presents, grids)

let trivial_upper_bound presents grids =
    (* consider being able to tile perfectly, aka just count the number of cells occupied *)
    let present_sizes =
        List.map presents ~f:(fun present ->
            List.sum
              (module Int)
              present
              ~f:(fun s -> s |> String.filter ~f:(Char.equal '#') |> String.length))
    in
    List.count grids ~f:(fun (size_x, size_y, required_presents) ->
        let num_occupied =
            List.fold2_exn required_presents present_sizes ~init:0 ~f:(fun acc num_p size ->
                acc + (num_p * size))
        in
        num_occupied <= size_x * size_y)

let trivial_lower_bound presents grids =
    (* consider every present to be full 3x3 (kind of) *)
    List.count grids ~f:(fun (size_x, size_y, required_presents) ->
        let num_occupied = 9 * List.sum (module Int) ~f:Fun.id required_presents in
        num_occupied <= size_x * size_y)

let inp = Aoc.read_to_list "day12"

let () =
    let presents, grids = inp |> parse in
    Printf.printf "Lower bound: %d\n" (trivial_lower_bound presents grids);
    Printf.printf "Upper bound: %d\n" (trivial_upper_bound presents grids)
