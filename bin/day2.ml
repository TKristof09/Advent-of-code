open Core

type cube = Red | Green | Blue
type game = {
    id: int;
    sets: (cube * int) list list
}

let parse_game s = 
    let game, sets = String.lsplit2_exn s ~on:':' in
    let _, id = String.lsplit2_exn game ~on:' ' in
    let id = Int.of_string id in
    let sets = String.split sets ~on:';' in 
    let sets = List.map sets ~f:(fun set ->
        let cubes = String.split set ~on:',' in 
        List.map cubes ~f:(fun cube ->
            let cube = String.drop_prefix cube 1 in (* drop space *)
            match String.lsplit2_exn cube ~on:' ' with
            | n, "blue" -> (Blue, Int.of_string n)
            | n, "red" -> (Red, Int.of_string n)
            | n, "green" -> (Green, Int.of_string n)
            | _ -> failwith "parse error"
        )
    ) in 
    {
        id;
        sets
    }


let check_game g = 
    let check_set set = 
        List.fold set ~init:true ~f:(fun acc (c, n) -> 
            match c with
            | Red -> acc && (n <= 12)
            | Green -> acc && (n <= 13)
            | Blue -> acc && (n <= 14)
        )
    in
    List.fold g.sets ~init:true ~f:(fun acc set -> acc && (check_set set))

let calc_game_min g = 
    let get_set_usage set = 
        List.fold set ~init:(0,0,0) ~f:(fun (r,g,b) (c, n) -> 
            match c with
            | Red -> (n,g,b)
            | Green -> (r,n,b)
            | Blue -> (r,g,n)
        )
    and find_max_usages usages =
        List.fold usages ~init:(0,0,0) ~f:(fun (mr,mg,mb) (r,g,b) -> (max mr r, max mg g, max mb b))
    in
    List.fold g.sets ~init:[] ~f:(fun acc set -> (get_set_usage set)::acc) 
    |> find_max_usages



(* let () =  *)
(*     let lines = Aoc.read_to_list "day2" in *)
(*     List.fold lines ~init:0 ~f:(fun acc line -> *)
(*         let game = parse_game line in *)
(*         let possible = check_game game in *)
(*         Printf.printf "%d : %b\n" game.id possible; *)
(*         if possible then *)
(*             acc + game.id *)
(*         else *)
(*             acc *)
(*     ) *)
(*     |> Printf.printf "\n%d\n"  *)



let () = 
    let lines = Aoc.read_to_list "day2" in
    List.fold lines ~init:0 ~f:(fun acc line ->
        let game = parse_game line in
        let (r,g,b) = calc_game_min game in
        acc + (r * g * b)
    )
    |> Printf.printf "\n%d\n" 


