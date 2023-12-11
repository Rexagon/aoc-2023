open Stdlib.Printf

let parse_universe scale lines =
  let fill_empty empty_items f list =
    list |> List.iteri (fun i item ->
      let is_empty = f item && Hashtbl.find_opt empty_items i |> Option.value ~default:true in
      Hashtbl.replace empty_items i is_empty);
  in

  let parse_line line = List.init (String.length line) (String.get line) in
  let lines = List.map (parse_line) lines in

  let empty_rows = Hashtbl.create (List.length lines) in
  fill_empty empty_rows (List.for_all ((=) '.')) lines;

  let empty_columns = Hashtbl.create (List.hd lines |> List.length) in
  lines |> List.iter (fill_empty empty_columns ((=) '.'));

  let get_galaxies_in_row row line =
    let rec get_galaxies_in_row' row col i line =
      match line with
      | [] -> []
      | x::line -> (
        let next_col = col + if Hashtbl.find empty_columns i then scale else 1 in
        let item = if x == '#' then [(row, col)] else [] in
        item @ get_galaxies_in_row' row next_col (i + 1) line
      )
    in get_galaxies_in_row' row 0 0 line
  in
  let get_galaxies lines =
    let rec get_galaxies' row i lines =
      match lines with
      | [] -> []
      | line::lines -> (
        let next_row = row + if Hashtbl.find empty_rows i then scale else 1 in
        get_galaxies_in_row row line @ get_galaxies' next_row (i + 1) lines
      )
    in get_galaxies' 0 0 lines
  in get_galaxies lines

let sum_distances scale lines =
  let galaxies = parse_universe scale lines in
  let find_distance (row, col) (row', col') = (abs (row' - row)) + (abs (col' - col)) in
  let rec find_distances galaxies =
    match galaxies with
    | [] -> []
    | galaxy::rest -> (
      let distances = List.map (find_distance galaxy) rest in
      distances @ find_distances rest
    )
  in
  find_distances galaxies |> List.fold_left (+) 0

let part1 = sum_distances 2
let part2 = sum_distances 1000000

let solve filename =
  let ic = open_in filename in
  let content = In_channel.input_all ic in
  let lines = String.split_on_char '\n' content in
  printf "%d\n" @@ part1 lines;
  printf "%d\n" @@ part2 lines

let () = solve "input.txt"
