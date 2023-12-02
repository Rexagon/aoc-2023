open Stdlib.Printf

let parse_game line =
  let parse_game_id s =
    match String.split_on_char ' ' s with
    | _::id::[] -> int_of_string id
    | _ -> raise (Invalid_argument "id")
  in
  let parse_cube_sets s =
    let parse_part part =
      match String.split_on_char ' ' part with
      | ""::n::color::[] -> (color, int_of_string n)
      | _ -> raise (Invalid_argument "part")
    in
    let parse_set set =
      let set = String.split_on_char ',' set in
      let add_part (red, green, blue) (color, n) =
        match color with
        | "red" -> (n, green, blue)
        | "green" -> (red, n, blue)
        | "blue" -> (red, green, n)
        | _ -> raise (Invalid_argument "color")
      in
      List.fold_left (fun acc part -> parse_part part |> add_part acc) (0, 0, 0) set
    in
    String.split_on_char ';' s |> List.map parse_set
  in
  match String.split_on_char ':' line with
  | first::second::[] -> (parse_game_id first, parse_cube_sets second)
  | _ -> raise (Invalid_argument "line")
;;

let fold_games_list lines f =
  List.map (fun line -> let (id, sets) = parse_game line in f id sets) lines |>
  List.fold_left (+) 0

let part1 lines max =
  let check_set (mr, mg, mb) (r, g, b) = r <= mr && g <= mg && b <= mb in
  let process_game id sets =
    if List.for_all (check_set max) sets then id
    else 0
  in fold_games_list lines process_game
;;

let part2 lines =
  let process_game _ sets =
    match sets with
    | [] -> 0
    | x::xs -> (
      let max_set (mr, mg, mb) (r, g, b) = (max mr r, max mg g, max mb b) in
      let (r, g, b) = List.fold_left (max_set) x xs in
      r * g * b
    )
  in fold_games_list lines process_game
;;

let solve filename =
  let ic = open_in filename in
  let content = In_channel.input_all ic in
  let lines = String.split_on_char '\n' content in
  printf "%d\n" @@ part1 lines (12, 13, 14);
  printf "%d\n" @@ part2 lines
;;

let () = solve "input.txt"
