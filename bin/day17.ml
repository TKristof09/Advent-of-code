open Core

type op =
    | ADV
    | BXL
    | BST
    | JNZ
    | BXC
    | OUT
    | BDV
    | CDV
[@@deriving show]

type register =
    | A
    | B
    | C
[@@deriving show]

type operand =
    | Imm of int
    | Reg of register
    | Ignored
[@@deriving show]

type program = {
    mutable a : int;
    mutable b : int;
    mutable c : int;
    instrs : (op * operand) array;
    mutable ip : int;
    mutable output : int list;
  }
[@@deriving show]

let rec parse_instrs l =
    let read_combo operand =
        match operand with
        | 0
        | 1
        | 2
        | 3 ->
            Imm operand
        | 4 -> Reg A
        | 5 -> Reg B
        | 6 -> Reg C
        | _ -> Ignored
    in
    match l with
    | [] -> []
    | [ _ ] -> failwith "Invalid instr length"
    | op :: operand :: t ->
        let c_operand = read_combo operand in
        let l_operand = Imm operand in
        let instr =
            match op with
            | 0 -> (ADV, c_operand)
            | 1 -> (BXL, l_operand)
            | 2 -> (BST, c_operand)
            | 3 -> (JNZ, l_operand)
            | 4 -> (BXC, Ignored)
            | 5 -> (OUT, c_operand)
            | 6 -> (BDV, c_operand)
            | 7 -> (CDV, c_operand)
            | _ -> failwith "Invalid op"
        in
        instr :: parse_instrs t

let reg_regex =
    let open Re in
    seq [ str "Register "; alpha; str ": "; group @@ rep digit ] |> compile

let parse_program inp =
    let r = Re.exec reg_regex inp.(0) in
    let a = Re.Group.get r 1 |> Int.of_string in

    let r = Re.exec reg_regex inp.(1) in
    let b = Re.Group.get r 1 |> Int.of_string in

    let r = Re.exec reg_regex inp.(2) in
    let c = Re.Group.get r 1 |> Int.of_string in

    let _, p = String.lsplit2_exn inp.(4) ~on:':' in
    let p =
        String.to_list p |> List.filter_map ~f:(fun c -> String.of_char c |> Int.of_string_opt)
    in
    let instrs = p |> parse_instrs |> Array.of_list in
    ({ a; b; c; instrs; ip = 0; output = [] }, p)

let run p stop_at_output =
    let read_operand p x =
        match x with
        | Imm i -> i
        | Reg r -> (
            match r with
            | A -> p.a
            | B -> p.b
            | C -> p.c)
        | Ignored -> failwith "Ignored operand"
    in
    let exec p instr =
        let ip = p.ip + 2 in
        p.ip <- ip;
        match instr with
        | ADV, x ->
            let denum = read_operand p x |> Int.pow 2 in
            let num = p.a in
            p.a <- num / denum
        | BXL, x ->
            let x = read_operand p x in
            p.b <- p.b lxor x
        | BST, x ->
            let x = read_operand p x in
            p.b <- x mod 8
        | JNZ, x ->
            let x = read_operand p x in
            assert (x mod 2 = 0);
            let ip = if p.a = 0 then ip else x in
            p.ip <- ip
        | BXC, Ignored -> p.b <- p.b lxor p.c
        | BXC, _ -> failwith "BXC not ignored"
        | OUT, x ->
            let x = read_operand p x in
            p.output <- (x mod 8) :: p.output
        | BDV, x ->
            let denum = read_operand p x |> Int.pow 2 in
            let num = p.a in
            p.b <- num / denum
        | CDV, x ->
            let denum = read_operand p x |> Int.pow 2 in
            let num = p.a in
            p.c <- num / denum
    in
    let n = Array.length p.instrs in
    (* the starting values of b and c don't matter for the input program *)
    p.b <- 0;
    p.c <- 0;
    p.ip <- 0;
    p.output <- [];
    while p.ip < n * 2 && not (stop_at_output && (not @@ List.is_empty p.output)) do
      exec p p.instrs.(p.ip / 2)
    done;
    List.rev p.output

type node =
    | Node of (int * node) list
    | Leaf
[@@deriving show]

let rec make_tree p output =
    match output with
    | [] -> Leaf
    | h :: t ->
        let children =
            Sequence.range 0 8
            |> Sequence.fold ~init:[] ~f:(fun acc i ->
                   let a = (p.a lsl 3) lor i in
                   let p = { p with a } in
                   let res = run p true in
                   if List.hd_exn res = h then
                     let p = { p with a } in
                     (i, make_tree p t) :: acc
                   else
                     acc)
        in
        if List.is_empty children then
          Leaf
        else
          Node children

let list_to_int l = List.fold l ~init:0 ~f:(fun acc x -> (acc lsl 3) lor x)

let find_paths tree n =
    let rec aux n current_path tree =
        match tree with
        | Leaf ->
            if n = 0 then
              [ List.rev current_path ]
            else
              []
        | Node children ->
            if n = 0 then
              [] (* Path too long already *)
            else
              List.concat
                (List.map children ~f:(fun (label, child) ->
                     aux (n - 1) (label :: current_path) child))
    in
    aux n [] tree

let () =
    let p1, expected = Aoc.read_to_array "day17" |> parse_program in
    let p2 = { p1 with a = 0 } in
    (* show_program p |> Printf.printf "%s\n"; *)
    let res = run p1 false in
    Printf.printf "Part 1: %s\n" ([%derive.show: int list] res);
    let l = List.rev expected in
    let root = make_tree p2 l in
    let paths = find_paths root (List.length l) in
    Printf.printf "Part 2: %d\n"
      (List.map paths ~f:list_to_int |> List.sort ~compare:Int.compare |> List.hd_exn)

(* let () = *)
(*     let p1, expected = Aoc.read_to_array "day17" |> parse_program in *)
(*     (* show_program p |> Printf.printf "%s\n"; *) *)
(*     for i = 0 to 255 do *)
(*       let res = run { p1 with a = i } false in *)
(*       Printf.printf "%16s -- %s\n" (Aoc.show_binary i) ([%derive.show: int list] res) *)
(*     done *)
