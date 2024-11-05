open Core

type range = {
      s: int
    ; l: int
  }
[@@deriving show]

type range_map = {
      s_start: int
    ; length: int
    ; d_start: int
  }
[@@deriving show]

type almanac = {
      seeds: int list
    ; seed_to_soil: range_map list
    ; soil_to_fert: range_map list
    ; fert_to_water: range_map list
    ; water_to_light: range_map list
    ; light_to_temp: range_map list
    ; temp_to_hum: range_map list
    ; hum_to_loc: range_map list
  }
[@@deriving show]

let default_almanac =
    {
      seeds= []
    ; seed_to_soil= []
    ; soil_to_fert= []
    ; fert_to_water= []
    ; water_to_light= []
    ; light_to_temp= []
    ; temp_to_hum= []
    ; hum_to_loc= []
    }

let parse_seeds line =
    let _, nums = String.lsplit2_exn line ~on:':' in
    String.split nums ~on:' ' |> List.filter_map ~f:Int.of_string_opt

let parse_maps lines =
    List.fold_until lines ~init:[]
      ~f:(fun acc line ->
        let nums = String.split line ~on:' ' |> List.filter_map ~f:Int.of_string_opt in
        match nums with
        | [ds; ss; l] -> Continue ({ s_start= ss; length= l; d_start= ds } :: acc)
        | _ -> Stop acc )
      ~finish:(fun acc -> acc)

let parse_almanac lines =
    let seeds = parse_seeds (List.hd_exn lines) in
    let rec aux lines almanac =
        match lines with
        | [] -> almanac
        | line :: t -> (
            let ranges =
                parse_maps t |> List.sort ~compare:(fun r r' -> Int.compare r.s_start r'.s_start)
            in
            match line with
            | "seed-to-soil map:" -> aux t { almanac with seed_to_soil= ranges }
            | "soil-to-fertilizer map:" -> aux t { almanac with soil_to_fert= ranges }
            | "fertilizer-to-water map:" -> aux t { almanac with fert_to_water= ranges }
            | "water-to-light map:" -> aux t { almanac with water_to_light= ranges }
            | "light-to-temperature map:" -> aux t { almanac with light_to_temp= ranges }
            | "temperature-to-humidity map:" -> aux t { almanac with temp_to_hum= ranges }
            | "humidity-to-location map:" -> aux t { almanac with hum_to_loc= ranges }
            | _ -> aux t almanac )
    in
    aux lines { default_almanac with seeds }

let map_num ranges n =
    List.fold_until ranges ~init:(-1)
      ~f:(fun _ range ->
        if n > range.s_start && n - range.s_start < range.length then
          Stop (n - range.s_start + range.d_start)
        else
          Continue (-1) )
      ~finish:(fun acc -> if acc = -1 then n else acc)

let map_range ranges_map range =
    List.fold_until ranges_map ~init:([], range)
      ~f:(fun (acc, cur_range) r ->
        if cur_range.l = 0 then
          Stop acc
        else if cur_range.s >= r.s_start && cur_range.s - r.s_start < r.length then
          let e = min (cur_range.s + cur_range.l) (r.s_start + r.length) in
          let l = e - cur_range.s in
          Continue
            ( { s= r.d_start + cur_range.s - r.s_start; l } :: acc
            , { s= cur_range.s + l; l= cur_range.l - l } )
        else
          Continue (acc, cur_range) )
      ~finish:(fun (acc, cur_range) -> if cur_range.l = 0 then acc else cur_range :: acc)

let map_ranges ranges_map ranges =
    List.fold ranges ~init:[] ~f:(fun acc range -> map_range ranges_map range @ acc)

let find_loc seed almanac =
    Printf.printf "Seed %d: [" seed ;
    let soil = map_num almanac.seed_to_soil seed in
    Printf.printf "%d, " soil ;
    let fert = map_num almanac.soil_to_fert soil in
    Printf.printf "%d, " fert ;
    let water = map_num almanac.fert_to_water fert in
    Printf.printf "%d, " water ;
    let light = map_num almanac.water_to_light water in
    Printf.printf "%d, " light ;
    let temp = map_num almanac.light_to_temp light in
    Printf.printf "%d, " temp ;
    let hum = map_num almanac.temp_to_hum temp in
    Printf.printf "%d, " temp ;
    let loc = map_num almanac.hum_to_loc hum in
    Printf.printf "%d]\n" loc ; loc

let find_loc_range seed_range almanac =
    Printf.printf "Seed %s: \n" (show_range seed_range) ;
    let soil = map_ranges almanac.seed_to_soil [seed_range] in
    Printf.printf "\tSoils:\n%s\n " ([%derive.show: range list] soil) ;
    let fert = map_ranges almanac.soil_to_fert soil in
    Printf.printf "\tFerst:\n%s\n " ([%derive.show: range list] fert) ;
    let water = map_ranges almanac.fert_to_water fert in
    Printf.printf "\tWaters:\n%s\n " ([%derive.show: range list] water) ;
    let light = map_ranges almanac.water_to_light water in
    Printf.printf "\tLights:\n%s\n " ([%derive.show: range list] light) ;
    let temp = map_ranges almanac.light_to_temp light in
    Printf.printf "\tTemps:\n%s\n " ([%derive.show: range list] temp) ;
    let hum = map_ranges almanac.temp_to_hum temp in
    Printf.printf "\tHums:\n%s\n " ([%derive.show: range list] hum) ;
    let loc = map_ranges almanac.hum_to_loc hum in
    Printf.printf "\tLocs:\n%s\n " ([%derive.show: range list] loc) ;
    loc

let rec get_seed_ranges seeds =
    match seeds with
    | [] -> []
    | [_] -> failwith "shouldnt happen"
    | s :: l :: t -> { s; l } :: get_seed_ranges t

let () =
    let lines = Aoc.read_to_list "day5" in
    let almanac = parse_almanac lines in
    print_endline @@ show_almanac almanac ;
    let seed_ranges = get_seed_ranges almanac.seeds in
    List.fold seed_ranges ~init:Int.max_value ~f:(fun lowest seed_range ->
        min lowest
        @@ List.fold (find_loc_range seed_range almanac) ~init:Int.max_value
             ~f:(fun range_min range -> min range_min range.s ) )
    |> Printf.printf "\n%d\n"
