open Core

type hand = {
      cards: char list
    ; bid: int
    ; score: int
  }
[@@deriving show]

type hand_type =
    | Five
    | Four
    | Full_house
    | Three
    | Two_pair
    | One_pair
    | High_card

let type_to_int t =
    match t with
    | Five -> 7
    | Four -> 6
    | Full_house -> 5
    | Three -> 4
    | Two_pair -> 3
    | One_pair -> 2
    | High_card -> 1

let score_with_joker occurences =
    match occurences with
    | [_] -> Five
    | (_, 4) :: _ -> Five
    | [(_, 3); (_, 2)] -> Five
    | (_, 3) :: (_, 1) :: _ -> Four
    | [(_, 2); (_, 2); ('J', 1)] -> Full_house
    | [(_, 2); (_, 2); (_, 1)] -> Four
    | (_, 2) :: _ -> Three
    | _ -> One_pair

let score_hand hand =
    let occurences =
        List.fold hand.cards ~init:Char.Map.empty ~f:(fun map c ->
            Map.update map c ~f:(fun num ->
                match num with
                | None -> 1
                | Some n -> n + 1 ) )
        |> Map.to_alist
        |> List.sort ~compare:(fun (_, n1) (_, n2) -> compare n2 n1)
    in
    let score =
        if List.mem hand.cards 'J' ~equal:Char.equal then
          score_with_joker occurences
        else
          match occurences with
          | [_] -> Five
          | (_, 4) :: _ -> Four
          | [(_, 3); (_, 2)] -> Full_house
          | [(_, 3); (_, 1); (_, 1)] -> Three
          | (_, 2) :: (_, 2) :: _ -> Two_pair
          | (_, 2) :: _ -> One_pair
          | _ -> High_card
    in
    { hand with score= type_to_int score }

let parse_hand line =
    let cards, bid = String.lsplit2_exn line ~on:' ' in
    let bid = Int.of_string bid in
    score_hand { cards= String.to_list cards; bid; score= 0 }

let compare_hand h1 h2 =
    let rec compare_cards l1 l2 =
        let card_to_int c =
            match c with
            | 'A' -> 13
            | 'K' -> 12
            | 'Q' -> 11
            | 'T' -> 10
            | 'J' -> 1
            | _ -> Char.get_digit_exn c
        in
        match (l1, l2) with
        | [], [] -> 0
        | c1 :: t1, c2 :: t2 -> (
            match compare (card_to_int c1) (card_to_int c2) with
            | 0 -> compare_cards t1 t2
            | x -> x )
        | _, _ -> failwith "shouldn't happen"
    in
    match compare h1.score h2.score with
    | 0 -> compare_cards h1.cards h2.cards
    | x -> x

let () =
    Aoc.read_to_list_filtered "day7"
    |> List.map ~f:parse_hand
    |> List.sort ~compare:compare_hand
    |> List.foldi ~init:0 ~f:(fun i acc hand -> acc + ((i + 1) * hand.bid))
    |> Printf.printf "%d\n"

let () =
    Printf.printf "%d\n" @@ Int.max 1 1 ;
    Printf.printf "\n"

(* List.iter ~f:(fun hand -> Printf.printf "%s\n" @@ show_hand hand) hands ; *)
