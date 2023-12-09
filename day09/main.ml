open Stdlib.Printf

type direction = Forward | Backward

let rec fold_diff numbers =
  match numbers with
  | [] | _::[] -> []
  | x1::x2::xs -> (x2 - x1) :: fold_diff (x2::xs)

let rec extrapolate dir line =
  if List.for_all ((=) 0) line then 0
  else
    let head, op = match dir with
    | Forward -> (List.rev line |> List.hd, (+))
    | Backward -> (List.hd line, (-))
    in fold_diff line |> extrapolate dir |> op head

let extrapolate_input dir lines =
  lines |>
  List.map (fun line ->
    String.split_on_char ' ' line |>
    List.map (int_of_string)) |>
  List.map (extrapolate dir) |>
  List.fold_left (+) 0

let part1 = extrapolate_input Forward
let part2 = extrapolate_input Backward

let solve filename =
  let ic = open_in filename in
  let content = In_channel.input_all ic in
  let lines = String.split_on_char '\n' content in
  printf "%d\n" @@ part1 lines;
  printf "%d\n" @@ part2 lines

let () = solve "input.txt"
