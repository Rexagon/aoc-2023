open Stdlib.Printf

let score_card joker_mode card =
  match card with
  | '2'..'9' -> Char.code card - Char.code '0'
  | 'T' -> 10
  | 'J' -> if joker_mode then -1 else 11
  | 'Q' -> 12
  | 'K' -> 13
  | 'A' -> 14
  | _ -> raise (Invalid_argument "card")

let compute_hand_type joker_mode cards =
  let tbl = Hashtbl.create 5 in
  cards |> List.iter
    (fun card ->
      (match Hashtbl.find_opt tbl card with
      | None -> 1
      | Some n -> n + 1)
      |> Hashtbl.replace tbl card);
  let jokers = match Hashtbl.find_opt tbl (-1) with
    | Some n when joker_mode -> n
    | _ -> 0
  in
  let hand =
    Hashtbl.to_seq_values tbl |>
    List.of_seq |>
    List.sort (compare)
  in match hand with
  | [1; 1; 1; 1; 1] -> (
    match jokers with
    | 1 -> 1 (* upgrade to one pair *)
    | _ -> 0 (* high card *)
  )
  | [1; 1; 1; 2] -> (
    match jokers with
    | 1 | 2 -> 3 (* upgrade to three of a kind *)
    | _ -> 1 (* one pair *)
  )
  | [1; 2; 2] -> (
    match jokers with
    | 1 -> 4 (* upgrade to full house *)
    | 2 -> 5 (* upgrade to four of a kind *)
    | _ -> 2 (* two pair *)
  )
  | [1; 1; 3] -> (
    match jokers with
    | 1 | 3 -> 5 (* upgrade to four of a kind *)
    | _ -> 3 (* three of a kind *)
  )
  | [2; 3] -> (
    match jokers with
    | 2 | 3 -> 6 (* upgrade to five of a kind *)
    | _ -> 4 (* full house *)
  )
  | [1; 4] -> (
    match jokers with
    | 1 | 4 -> 6 (* upgrade to five of a kind *)
    | _ -> 5 (* four of a kind *)
  )
  | [5] -> 6
  | _ -> raise (Invalid_argument "hand")

let compare_hands (left_ty, left_cards) (right_ty, right_cards) =
  match compare left_ty right_ty with
  | x when x != 0 -> x
  | _ -> List.compare (compare) left_cards right_cards

let compute_total_winnings joker_mode lines =
  let parse_hand hand =
    let cards =
      List.init (String.length hand) (String.get hand) |>
      List.map (score_card joker_mode)
    in (compute_hand_type joker_mode cards, cards)
  in
  let parse_line line =
    match String.split_on_char ' ' line with
    | hand::bid::[] -> (parse_hand hand, int_of_string bid)
    | _ -> raise (Invalid_argument "line")
  in
  let (_, sum) =
    List.map (parse_line) lines |>
    List.sort (fun (left_hand, _) (right_hand, _) -> compare_hands left_hand right_hand) |>
    List.fold_left (fun (i, sum) (_, bid) -> (i + 1, sum + bid * i)) (1, 0)
  in sum

let part1 = compute_total_winnings false
let part2 = compute_total_winnings true

let solve filename =
  let ic = open_in filename in
  let content = In_channel.input_all ic in
  let lines = String.split_on_char '\n' content in
  printf "%d\n" @@ part1 lines;
  printf "%d\n" @@ part2 lines
;;

let () = solve "input.txt"
