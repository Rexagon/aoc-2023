open Stdlib.Printf

let parse_input lines =
  let parse_line line =
    match String.split_on_char ' ' line with
    | springs::groups::[] -> (
      let springs = List.init (String.length springs) (String.get springs) in
      let groups = String.split_on_char ',' groups |> List.map (int_of_string) in
      (springs, groups)
    )
    | _ -> raise (Invalid_argument "line")
  in List.map (parse_line) lines

let count_arrangements (springs, groups) =
  let cache = Hashtbl.create 1000 in
  let rec count_ways' springs groups current =
    match springs with
    | [] -> (
      match current, groups with
      | None, [] -> 1
      | Some current, last::[] when current = last -> 1
      | _ -> 0
    )
    | s::rem_springs -> (
      let key = (List.length springs, Option.value current ~default:0, List.length groups) in
      match Hashtbl.find_opt cache key with
      | Some n -> n
      | None -> (
        let n = match s, current, groups with
          | _, Some _, [] -> 0
          | '.', Some x, next::_ when x != next -> 0
          | '.', Some _, _::rem_groups -> count_ways' rem_springs rem_groups None
          | '.', None, _ -> count_ways' rem_springs groups None
          | '#', Some x, _ -> count_ways' rem_springs groups (Some (x + 1))
          | '#', None, _ -> count_ways' rem_springs groups (Some 1)
          | '?', Some x, group::rem_groups -> (
            let res = count_ways' rem_springs groups (Some (x + 1)) in
            res +
              if x <> group then 0
              else count_ways' rem_springs rem_groups None
          )
          | '?', None, _ -> (
            count_ways' rem_springs groups (Some 1) +
            count_ways' rem_springs groups None
          )
          | _ -> raise (Invalid_argument "symbol")
        in
        Hashtbl.replace cache key n;
        n
      )
    )
  in count_ways' springs groups None

let part1 lines =
  parse_input lines |>
  List.map count_arrangements |>
  List.fold_left (+) 0

let part2 lines =
  let rec preprocess count (springs, groups) =
    if count <= 1 then (springs, groups)
    else
      let (rest_springs, rest_groups) = preprocess (count - 1) (springs, groups) in
      (springs @ ('?'::rest_springs), groups @ rest_groups)
  in
  parse_input lines |>
  List.map (preprocess 5) |>
  List.map count_arrangements |>
  List.fold_left (+) 0

let solve filename =
  let ic = open_in filename in
  let content = In_channel.input_all ic in
  let lines = String.split_on_char '\n' content in
  printf "%d\n" @@ part1 lines;
  printf "%d\n" @@ part2 lines

let () = solve "input.txt"
