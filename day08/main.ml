open Stdlib.Printf

type direction = Left | Right

let parse_lines lines =
  let split_and_trim delim line =
    String.split_on_char delim line |>
    List.map (String.trim)
  in
  let parse_path line =
    List.init
      (String.length line)
      (fun i ->
        match String.get line i with
        | 'L' -> Left
        | 'R' -> Right
        | _ -> raise (Invalid_argument "path"))
  in
  let parse_location line =
    match split_and_trim '=' line with
    | loc::dirs::[] -> (
      let dirs = match String.sub dirs 1 8 |> split_and_trim ',' with
        | left::right::[] -> (left, right)
        | _ -> raise (Invalid_argument "directions")
      in (loc, dirs)
    )
    | _ -> raise (Invalid_argument "location")
  in
  let parse_locations lines =
    List.map (parse_location) lines |>
    List.fold_left
      (fun tbl (loc, dirs) ->
        Hashtbl.replace tbl loc dirs; tbl)
      (Hashtbl.create 1000)
  in
  match lines with
  | path::""::locations ->
    (parse_path path, parse_locations locations)
  | _ -> raise (Invalid_argument "lines")

let follow_path orig_path locations f start =
  let rec follow_path loc path len =
    match path with
    | _ when f loc -> len
    | [] -> follow_path loc orig_path len
    | dir::path -> (
      let (left, right) = Hashtbl.find locations loc in
      let loc = match dir with Left -> left | Right -> right in
      follow_path loc path (len + 1)
    )
  in follow_path start orig_path 0

let part1 lines =
  let (orig_path, locations) = parse_lines lines in
  follow_path orig_path locations ((=) "ZZZ") "AAA"

let part2 lines =
  let (orig_path, locations) = parse_lines lines in
  let ends_with c line = c = String.get line 2 in
  let distances =
    Hashtbl.to_seq_keys locations |>
    Seq.filter (ends_with 'A') |>
    Seq.map (follow_path orig_path locations (ends_with 'Z')) |>
    List.of_seq
  in
  let rec gcd a b =
    if b = 0 then a
    else gcd b (a mod b)
  in
  let lcm a b = a * b / gcd a b in
  List.fold_left lcm (List.hd distances) (List.tl distances)

let solve filename =
  let ic = open_in filename in
  let content = In_channel.input_all ic in
  let lines = String.split_on_char '\n' content in
  printf "%d\n" @@ part1 lines;
  printf "%d\n" @@ part2 lines

let () = solve "input.txt"
