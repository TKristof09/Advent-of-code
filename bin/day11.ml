open Core

let parse l =
    let htbl = Hashtbl.create ~size:100 (module String) in
    List.iter l ~f:(fun s ->
        let start, neighbours = String.lsplit2_exn s ~on:':' in
        let neighbours = String.lstrip neighbours |> String.split ~on:' ' in
        Hashtbl.set htbl ~key:start ~data:neighbours);
    htbl

let rec dfs graph cache cur goal =
    match Hashtbl.find cache cur with
    | Some i -> i
    | None ->
        if String.equal cur goal then
          1
        else
          let num_paths =
              Hashtbl.find graph cur
              |> Option.value ~default:[]
              |> List.sum (module Int) ~f:(fun node -> dfs graph cache node goal)
          in
          Hashtbl.set cache ~key:cur ~data:num_paths;
          num_paths

let inp = Aoc.read_to_list "day11"

let part1 () =
    let graph = parse inp in
    dfs graph (Hashtbl.create ~size:1000 (module String)) "you" "out"

let part2 () =
    let graph = parse inp in
    let dac_fft = dfs graph (Hashtbl.create ~size:1000 (module String)) "dac" "fft" in
    let svr_dac =
        if dac_fft > 0 then dfs graph (Hashtbl.create ~size:1000 (module String)) "svr" "dac" else 0
    in
    let fft_out =
        if svr_dac > 0 then dfs graph (Hashtbl.create ~size:1000 (module String)) "fft" "out" else 0
    in

    let fft_dac = dfs graph (Hashtbl.create ~size:1000 (module String)) "fft" "dac" in
    let svr_fft =
        if fft_dac > 0 then dfs graph (Hashtbl.create ~size:1000 (module String)) "svr" "fft" else 0
    in
    let dac_out =
        if svr_fft > 0 then dfs graph (Hashtbl.create ~size:1000 (module String)) "dac" "out" else 0
    in
    (svr_fft * fft_dac * dac_out) + (svr_dac * dac_fft * fft_out)

let () =
    part1 () |> Printf.printf "Part 1: %d\n";
    part2 () |> Printf.printf "Part 2: %d\n"

let () =
    Aoc.time_fn part1;
    Aoc.time_fn part2
