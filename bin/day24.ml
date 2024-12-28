open Core

type op =
    | AND
    | OR
    | XOR

type circuit = {
    inputs : int Map.M(String).t;
    gates : (op * string * string) Map.M(String).t;
    outputs : string list;
  }

let parse it =
    let op_of_string = function
        | "AND" -> AND
        | "OR" -> OR
        | "XOR" -> XOR
        | _ -> failwith "Error parsing op"
    in
    let inputs =
        IterLabels.head_exn it
        |> List.fold ~init:String.Map.empty ~f:(fun acc s ->
               let wire, value = String.lsplit2_exn s ~on:':' |> Tuple2.map ~f:String.strip in
               Map.set acc ~key:wire ~data:(Int.of_string value))
    in
    let gates, outputs =
        IterLabels.drop 1 it
        |> IterLabels.head_exn
        |> List.fold ~init:(String.Map.empty, []) ~f:(fun (m, outputs) s ->
               let l = String.split s ~on:' ' in
               match l with
               | [ w1; op; w2; _; out ] ->
                   let op = op_of_string op in
                   let m = Map.set m ~key:out ~data:(op, w1, w2) in
                   if Char.equal out.[0] 'z' then
                     (m, out :: outputs)
                   else
                     (m, outputs)
               | _ -> failwith "Error")
    in
    let outputs = List.sort outputs ~compare:String.compare in
    { inputs; gates; outputs }

let solve circuit =
    let rec aux wire =
        match Map.find circuit.inputs wire with
        | Some x -> x
        | None -> (
            let op, w1, w2 = Map.find_exn circuit.gates wire in
            let w1 = aux w1
            and w2 = aux w2 in
            match op with
            | AND -> w1 land w2
            | OR -> w1 lor w2
            | XOR -> w1 lxor w2)
    in
    List.foldi circuit.outputs ~init:0 ~f:(fun i acc s -> (aux s lsl i) lor acc)

let print circuit =
    let rec aux wire =
        match Map.find circuit.inputs wire with
        | Some _ -> wire
        | None -> (
            let op, w1, w2 = Map.find_exn circuit.gates wire in
            let w1 = aux w1
            and w2 = aux w2 in
            match op with
            | AND -> Printf.sprintf "(%s AND %s)" w1 w2
            | OR -> Printf.sprintf "(%s OR %s)" w1 w2
            | XOR -> Printf.sprintf "(%s XOR %s)" w1 w2)
    in
    List.iter circuit.outputs ~f:(fun s -> Printf.printf "%s = %s\n" s (aux s))

let print_dot circuit =
    let buf = Buffer.create 1024 in
    Buffer.add_string buf "digraph Circuit {\n";

    let get_node_name wire =
        wire
        (* match Map.find !node_map wire with *)
        (* | Some name -> name *)
        (* | None -> *)
        (*     let name = "n" ^ string_of_int (get_node_id ()) in *)
        (*     node_map := Map.set !node_map ~key:wire ~data:name; *)
        (*     name *)
    in

    let add_input_node wire =
        let name = get_node_name wire in
        Printf.bprintf buf "  %s [label=\"%s\", shape=box];\n" name wire
    in

    let add_gate_node op w1 w2 out_wire =
        let name = get_node_name out_wire in
        let op_label =
            match op with
            | AND -> "AND"
            | OR -> "OR"
            | XOR -> "XOR"
        in
        Printf.bprintf buf "  %s [label=\"%s\"];\n" name op_label;
        Printf.bprintf buf "  %s -> %s;\n" (get_node_name w1) name;
        Printf.bprintf buf "  %s -> %s;\n" (get_node_name w2) name
    in

    let add_output_node wire =
        let name = get_node_name wire in
        let out_name = get_node_name ("out" ^ wire) in
        Printf.bprintf buf "  %s -> %s; \n" name out_name;
        Printf.bprintf buf "  %s [label=\"%s\", shape=doublecircle, color=\"red\"];\n" out_name wire
    in

    Map.iteri circuit.inputs ~f:(fun ~key ~data:_ -> add_input_node key);
    Map.iteri circuit.gates ~f:(fun ~key ~data:(op, w1, w2) -> add_gate_node op w1 w2 key);
    List.iter circuit.outputs ~f:add_output_node;

    Buffer.add_string buf "}\n";
    Buffer.contents buf |> Printf.printf "%s\n"

let cache = Hashtbl.create (module Int)

let calc a b circuit =
    let a = a land (Int.pow 2 45 - 1) in
    let b = b land (Int.pow 2 45 - 1) in
    let al = List.init 45 ~f:(fun i -> (1 lsl i) land a) in
    let bl = List.init 45 ~f:(fun i -> (1 lsl i) land b) in
    let m =
        List.foldi al ~init:String.Map.empty ~f:(fun i acc x ->
            let key = Printf.sprintf "x%02d" i in
            Map.set acc ~key ~data:(if x <> 0 then 1 else 0))
    in
    let m =
        List.foldi bl ~init:m ~f:(fun i acc x ->
            let key = Printf.sprintf "y%02d" i in
            Map.set acc ~key ~data:(if x <> 0 then 1 else 0))
    in
    let res = solve { circuit with inputs = m } in
    if a + b <> res then
      let s1 = Aoc.show_binary (a + b) in
      let s2 = Aoc.show_binary res in
      let i = String.common_suffix2_length s1 s2 in
      let s = Printf.sprintf "ERROR at %d:\n\tHAD: %s\n\tGOT: %s\n\n" i s1 s2 in
      let _ = Hashtbl.add cache ~key:i ~data:s in
      ()

let () =
    Aoc.read_to_iter "day24"
    |> Aoc.split_iter ~on:String.is_empty
    |> parse
    |> solve
    |> Printf.printf "Part 1: %d\n"

(* let () = Aoc.read_to_iter "day24" |> Aoc.split_iter ~on:String.is_empty |> parse |> print_dot *)
(* let () = *)
(*     let c = Aoc.read_to_iter "day24" |> Aoc.split_iter ~on:String.is_empty |> parse in *)
(*     let start = Int.pow 2 28 - 10 in *)
(*     for i = start to start + 15 do *)
(*       for j = 0 to 100 do *)
(*         calc i j c *)
(*       done *)
(*     done; *)
(*     Hashtbl.iter cache ~f:(fun s -> Printf.printf "%s\n" s) *)
